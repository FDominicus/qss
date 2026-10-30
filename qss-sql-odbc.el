;;; qss-sql-odbc.el --- ODBC databases as a source: the words become SQL  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; A table of any ODBC database is a source like an SQLite table, with
;; the same words; they become SQL in the dialect of the database:
;;
;;   (qss-> (odbc "test"))                          ; the tables of DSN test
;;   (qss-> (odbc "test" "orders")
;;          (where (> :amount 100))
;;          (sort-by order-date :desc)
;;          (first 20))
;;   (qss-> (files "~") (where (> :size "10mb")) (into odbc "test" "big_files"))
;;
;; One connection per DSN, opened on first use and kept (`qss-odbc-close'
;; closes them).  The schema is read once; `qss-odbc-refresh' reads it
;; again (writing with `into' does that by itself).
;;
;; Tested with PostgreSQL (psqlODBC Unicode) and SQLite (SQLite3 ODBC).
;; Other databases get the dialect of PostgreSQL, which is standard SQL
;; except for LIMIT; SQL Server, Access and MySQL need their own.
;;
;; Values: NUMERIC with digits after the point become floats in qss
;; tables (avg() in PostgreSQL has 20 of them); exact values come from
;; `qss-odbc-select'.  Conditions are SQL, so they compare exactly.
;; Editing rows (qss-edit) works on SQLite files only.
;;
;; The calls into ODBC come from the package qss-odbc
;; (https://github.com/FDominicus/qss-odbc).  It is loaded on first use;
;; without it qss works as before, and (odbc ...) says what is missing.

;;; Code:

(require 'cl-lib)
(require 'qss-sql)

(defun qss-sql-odbc--require ()
  "Load qss-odbc, or signal a `user-error' saying where to get it."
  (unless (require 'qss-odbc nil t)
    (user-error "ODBC needs the package qss-odbc (https://github.com/FDominicus/qss-odbc)")))

(declare-function qss-odbc-connect "qss-odbc")
(declare-function qss-odbc-connected-p "qss-odbc")
(declare-function qss-odbc-disconnect "qss-odbc")
(declare-function qss-odbc-execute "qss-odbc")
(declare-function qss-odbc-query "qss-odbc")
(declare-function qss-odbc-tables "qss-odbc")
(declare-function qss-odbc-table-columns "qss-odbc")
(declare-function qss-odbc-connection-dbms "qss-odbc")
(declare-function qss-odbc-connection-handle "qss-odbc")
(declare-function qss-odbc--column "qss-odbc")
(declare-function qss-odbc--value "qss-odbc")
(declare-function qss-odbc--params "qss-odbc")
(declare-function qss-odbc--exec "qss-odbc-module")
(declare-function qss-odbc--columns "qss-odbc-module")
(declare-function qss-odbc--fetch "qss-odbc-module")
(declare-function qss-odbc--close "qss-odbc-module")
(declare-function qss-odbc--commit "qss-odbc-module")
(declare-function qss-odbc--rollback "qss-odbc-module")
(declare-function qss-odbc--set-autocommit "qss-odbc-module")

(cl-defstruct (qss-sql-odbc (:constructor qss-sql-odbc--make) (:copier nil))
	      "An ODBC database as the database of queries.
DSN is the data source name (or connection string), CONNECTION the open
`qss-odbc-connection', SCHEMA the cached tables (NAME . COLUMNS),
KINDS an alist of table name -> \"TABLE\" or \"VIEW\"."
	      dsn connection schema kinds)

(defvar qss-sql-odbc--databases (make-hash-table :test #'equal)
  "DSN -> `qss-sql-odbc', one per data source.")

(defun qss-sql-odbc-database (dsn)
  "Return the database for DSN, connecting if needed."
  (qss-sql-odbc--require)
  (let ((db (or (gethash dsn qss-sql-odbc--databases)
                (puthash dsn (qss-sql-odbc--make :dsn dsn) qss-sql-odbc--databases))))
    (qss-sql-odbc--connection db)
    db))

(defun qss-sql-odbc--connection (db)
  "Return the open connection of DB, opening it again if it was closed."
  (let ((con (qss-sql-odbc-connection db)))
    (unless (and con (qss-odbc-connected-p con))
      (setf con (qss-odbc-connect (qss-sql-odbc-dsn db))
            (qss-sql-odbc-connection db) con
            (qss-sql-odbc-schema db) nil))
    con))

(defun qss-odbc-close (&optional dsn)
  "Close the connection to DSN that qss queries use; without DSN, all."
  (interactive)
  (maphash (lambda (k db)
             (when (and (or (null dsn) (equal k dsn))
                        (qss-sql-odbc-connection db)
                        (qss-odbc-connected-p (qss-sql-odbc-connection db)))
               (qss-odbc-disconnect (qss-sql-odbc-connection db))))
           qss-sql-odbc--databases))

(defun qss-odbc-refresh (&optional dsn)
  "Forget the schema read from DSN (all without DSN); it is read again on use."
  (interactive)
  (maphash (lambda (k db)
             (when (or (null dsn) (equal k dsn))
               (setf (qss-sql-odbc-schema db) nil (qss-sql-odbc-kinds db) nil)))
           qss-sql-odbc--databases))

(defun qss-sql-odbc--dbms (db)
  "The name of the database system of DB, in lower case."
  (downcase (qss-odbc-connection-dbms (qss-sql-odbc--connection db))))

(defun qss-sql-odbc--sqlite-p (db)
  (string-match-p "sqlite" (qss-sql-odbc--dbms db)))

;;; ------------------------------------------------------------------
;;; The generic functions of qss-sql.el

(defconst qss-sql-odbc--ascii-upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconst qss-sql-odbc--ascii-lower "abcdefghijklmnopqrstuvwxyz")

(defconst qss-sql-odbc--standard-dialect
  (list :true "(1=1)" :false "(1=0)"
        :is "IS NOT DISTINCT FROM" :is-not "IS DISTINCT FROM"
        :null-last "CASE WHEN %s IS NULL THEN 1 ELSE 0 END, %s"
        :null-first-desc "CASE WHEN %s IS NULL THEN 1 ELSE 0 END DESC, %s DESC"
        ;; LIKE folds ASCII only, as in SQLite (and so in memory)
        :like (format "(translate(%%s, '%s', '%s') LIKE translate(%%s, '%s', '%s'))"
                      qss-sql-odbc--ascii-upper qss-sql-odbc--ascii-lower
                      qss-sql-odbc--ascii-upper qss-sql-odbc--ascii-lower)
        ;; text by code points, whatever the collation of the database
        :binary-text "%s COLLATE \"C\"")
  "Standard SQL as PostgreSQL has it.")

(cl-defmethod qss-sql-db-dialect ((db qss-sql-odbc))
  "SQLite through ODBC speaks SQLite; the others standard SQL."
  (if (qss-sql-odbc--sqlite-p db) qss-sql--sqlite-dialect qss-sql-odbc--standard-dialect))

(cl-defmethod qss-sql-db-label ((db qss-sql-odbc))
  "The DSN."
  (qss-sql-odbc-dsn db))

(cl-defmethod qss-sql-db-value ((_db qss-sql-odbc) value)
  "Times as text (PostgreSQL compares text with DATE and TIMESTAMP), symbols
by name; t stays t (a BOOLEAN)."
  (cond ((qss--time-p value) (format-time-string qss-time-format value))
        ((and (symbolp value) value (not (eq value t))) (symbol-name value))
        (t value)))

(defun qss-sql-odbc--value (raw col)
  "The raw value RAW of the column COL for a qss table.
NUMERIC with digits after the point becomes a float, without an integer."
  (if (and (stringp raw) (memq (plist-get col :type) '(numeric decimal)))
      (if (string-match-p "\\`[-+]?[0-9]+\\'" raw)
          (string-to-number raw)
        (float (string-to-number raw)))
    (qss-odbc--value raw col)))

(cl-defmethod qss-sql-db-select ((db qss-sql-odbc) sql params)
  "Rows as lists of values for qss tables."
  (let ((stmt (qss-odbc--exec (qss-odbc-connection-handle (qss-sql-odbc--connection db))
                              sql (qss-odbc--params params))))
    (unwind-protect
        (let ((cols (mapcar #'qss-odbc--column (qss-odbc--columns stmt)))
              (rows nil) raw)
          (while (setq raw (qss-odbc--fetch stmt))
            (push (cl-mapcar #'qss-sql-odbc--value raw cols) rows))
          (nreverse rows))
      (qss-odbc--close stmt))))

(cl-defmethod qss-sql-db-rows ((db qss-sql-odbc) _rel run keys)
  "Rows as property lists; no rowids (no editing)."
  (let ((q (qss-sql-query run)))
    (mapcar (lambda (r) (cl-mapcan #'list keys r))
            (qss-sql-db-select db (car q) (cdr q)))))

(cl-defmethod qss-sql-db-order-id ((db qss-sql-odbc) table)
  "PostgreSQL: ctid of a table (the order a plain SELECT gives);
SQLite: rowid; others: none."
  (qss-sql-db-schema db)                ; reads the kinds too
  (and (equal (cdr (assoc table (qss-sql-odbc-kinds db))) "TABLE")
       (let ((dbms (qss-sql-odbc--dbms db)))
         (cond ((string-match-p "postgres" dbms) "ctid")
               ((string-match-p "sqlite" dbms) "rowid")))))

(cl-defmethod qss-sql-db-schema ((db qss-sql-odbc))
  "Tables and views through the catalog functions, cached."
  (or (qss-sql-odbc-schema db)
      (let* ((con (qss-sql-odbc--connection db))
             (rows (cl-remove-if
                    (lambda (r) (or (equal (plist-get r :table_name) qss-sql--types-table)
                                    (string-prefix-p "sqlite_" (plist-get r :table_name))))
                    (append (qss-odbc-tables con "TABLE") (qss-odbc-tables con "VIEW"))))
             (qss-types (qss-sql-odbc--read-types db))
             (tables
              (mapcar (lambda (r)
                        (let ((name (plist-get r :table_name)))
                          (cons name
                                (mapcar (lambda (c)
                                          (let ((col (plist-get c :column_name)))
                                            (list (qss-sql-name-key col) col
                                                  (or (plist-get c :type_name) "")
                                                  (cdr (assoc (cons (downcase name) col) qss-types)))))
                                        (qss-odbc-table-columns con name)))))
                      rows)))
        (dolist (tab tables) (qss-sql--check-collisions (car tab) (cdr tab)))
        (setf (qss-sql-odbc-kinds db)
              (mapcar (lambda (r) (cons (plist-get r :table_name) (plist-get r :table_type))) rows))
        (setf (qss-sql-odbc-schema db)
              (sort tables (lambda (a b) (string< (car a) (car b))))))))

(defun qss-sql-odbc--read-types (db)
  "The qss types recorded by `into' in DB: ((TABLE . COLUMN) . TYPE) ..."
  (let ((con (qss-sql-odbc--connection db)))
    (when (cl-some (lambda (r) (equal (plist-get r :table_name) qss-sql--types-table))
                   (qss-odbc-tables con "TABLE"))
      (mapcar (lambda (row) (cons (cons (downcase (nth 0 row)) (nth 1 row)) (intern (nth 2 row))))
              (cdr (qss-odbc-query con (format "SELECT \"table\", \"column\", \"type\" FROM %s"
                                               (qss-sql-quote qss-sql--types-table))))))))

;;; ------------------------------------------------------------------
;;; Source: (odbc DSN) and (odbc DSN TABLE)

;;;###autoload
(defun qss-odbc-source (dsn &optional table)
  "Return the table TABLE of the ODBC data source DSN as a query.
Without TABLE, the tables of DSN; RET on one shows its rows.  DSN is a
data source name or a connection string."
  (let ((db (qss-sql-odbc-database dsn)))
    (if table
        (qss-sql-relation db (if (symbolp table) (symbol-name table) table))
      (qss-sql-db-tables db))))

;;; ------------------------------------------------------------------
;;; Writing: (into odbc DSN NAME [:replace | :append])

(defun qss-sql-odbc--sql-type (db sqlite-type)
  "The column type for SQLITE-TYPE (INTEGER, REAL, TEXT) in DB."
  (if (qss-sql-odbc--sqlite-p db)
      sqlite-type
    (pcase sqlite-type
      ("INTEGER" "BIGINT")
      ("REAL" "DOUBLE PRECISION")
      (_ "TEXT"))))

(defun qss-sql-odbc-write (table db name &optional mode)
  "Write the rows of TABLE into the table NAME of the ODBC database DB.
As `qss-sql-write': MODE `replace' or `append' if NAME exists; the qss
types of the columns are recorded.  Return the number of rows written."
  (let* ((con (qss-sql-odbc--connection db))
         (specs (qss-sql--stored-specs table))
         (rows (qss-table-rows table))
         (quoted (qss-sql-quote name))
         (types-table (qss-sql-quote qss-sql--types-table))
         (tables (mapcar (lambda (r) (plist-get r :table_name)) (qss-odbc-tables con "TABLE")))
         (exists (cl-find name tables :test #'string-equal-ignore-case)))
    (unless specs (user-error "Nothing to store: the table has no columns"))
    (when (and exists (null mode))
      (user-error "Table %s exists in %s: add :replace or :append" name (qss-sql-odbc-dsn db)))
    ;; one transaction (as `qss-odbc-with-transaction', which is a macro
    ;; of qss-odbc and so not usable before qss-odbc is loaded)
    (let ((handle (qss-odbc-connection-handle con)) (ok nil))
      (qss-odbc--set-autocommit handle nil)
      (unwind-protect
	  (progn
	    (when (and exists (eq mode 'replace))
              (qss-odbc-execute con (format "DROP TABLE %s" (qss-sql-quote exists))))
	    (unless (and exists (eq mode 'append))
              (let ((types (mapcar (lambda (spec)
				     (qss-sql--column-types
                                      spec (mapcar (lambda (r) (qss-field r (car spec))) rows)))
				   specs)))
		(qss-odbc-execute con (format "CREATE TABLE %s (%s)" quoted
                                              (mapconcat (lambda (st)
							   (format "%s %s" (qss-sql-quote (symbol-name (car (car st))))
								   (qss-sql-odbc--sql-type db (cdr st))))
							 (cl-mapcar #'cons specs (mapcar #'car types))
							 ", ")))
		(unless (cl-find qss-sql--types-table tables :test #'equal)
		  (qss-odbc-execute con (format "CREATE TABLE %s (\"table\" VARCHAR(255), \"column\" VARCHAR(255),
                                             \"type\" VARCHAR(40), PRIMARY KEY (\"table\", \"column\"))"
						types-table)))
		(qss-odbc-execute con (format "DELETE FROM %s WHERE lower(\"table\") = lower(?)" types-table)
				  name)
		(cl-mapc (lambda (spec type)
			   (when (cdr type)
			     (qss-odbc-execute con (format "INSERT INTO %s VALUES (?, ?, ?)" types-table)
                                               name (symbol-name (car spec)) (symbol-name (cdr type)))))
			 specs types)))
	    (let ((insert (format "INSERT INTO %s (%s) VALUES (%s)" quoted
				  (mapconcat (lambda (s) (qss-sql-quote (symbol-name (car s)))) specs ", ")
				  (mapconcat (lambda (_) "?") specs ", "))))
              (dolist (row rows)
		(apply #'qss-odbc-execute con insert
                       (mapcar (lambda (spec) (qss-sql--store-value (qss-field row (car spec)) spec))
                               specs))))
	    (qss-odbc--commit handle)
	    (setq ok t))
	(unless ok (qss-odbc--rollback handle))
	(qss-odbc--set-autocommit handle t)))
    (setf (qss-sql-odbc-schema db) nil (qss-sql-odbc-kinds db) nil)
    (length rows)))

(cl-defmethod qss-into (table (_target (eql odbc)) &rest args)
  "Store TABLE in an ODBC database and return the stored table as a query.
ARGS: DSN, NAME, then :replace or :append.  Without them NAME must
not exist yet."
  (let ((dsn (pop args)))
    (unless (stringp dsn)
      (user-error "into odbc: first the data source, e.g. (into odbc \"test\" \"files\")"))
    (pcase-let* ((`(,name ,_file ,mode) (qss-sql--into-args args))
                 (db (qss-sql-odbc-database dsn)))
      (qss-sql-odbc-write (qss-as-table table) db name mode)
      (qss-sql-relation db name))))

;;; ------------------------------------------------------------------
;;; Registering with qss->

(setf (alist-get 'odbc qss-sources) 'qss-odbc-source)

(provide 'qss-sql-odbc)
;;; qss-sql-odbc.el ends here
