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
(declare-function qss-odbc-primary-key "qss-odbc")
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
KINDS an alist of table name -> property list (:type \"TABLE\" or
\"VIEW\" :rowid HAS-ROWID :schema SCHEMA :table NAME-IN-SCHEMA :qualified
T-IF-NAMED-SCHEMA.TABLE), KEYS an alist of table name -> its key columns
\(`qss-sql-db-key'), OPTIONS the options of `qss-odbc-connect' (:user,
:password)."
	      dsn connection schema kinds keys options)

(defconst qss-sql-odbc--rowid "_qss_rowid"
  "Hidden column that `into odbc' adds on MariaDB/MySQL for the input order.
INVISIBLE, so SELECT * does not show it (MariaDB 10.3, MySQL 8.0.23).")

(defvar qss-sql-odbc--databases (make-hash-table :test #'equal)
  "DSN -> `qss-sql-odbc', one per data source.")

(defun qss-sql-odbc-database (dsn &rest options)
  "Return the database for DSN, connecting if needed.
OPTIONS: :user and :password, as for `qss-odbc-connect'; given again
with other values, the connection is opened again with them."
  (qss-sql-odbc--require)
  (let ((db (or (gethash dsn qss-sql-odbc--databases)
                (puthash dsn (qss-sql-odbc--make :dsn dsn :options options)
                         qss-sql-odbc--databases))))
    (when (and options (not (equal options (qss-sql-odbc-options db))))
      (setf (qss-sql-odbc-options db) options)
      (when (and (qss-sql-odbc-connection db)
                 (qss-odbc-connected-p (qss-sql-odbc-connection db)))
        (qss-odbc-disconnect (qss-sql-odbc-connection db))))
    (qss-sql-odbc--connection db)
    db))

(defun qss-sql-odbc--connection (db)
  "Return the open connection of DB, opening it again if it was closed."
  (let ((con (qss-sql-odbc-connection db)))
    (unless (and con (qss-odbc-connected-p con))
      (setf con (apply #'qss-odbc-connect (qss-sql-odbc-dsn db) (qss-sql-odbc-options db))
            (qss-sql-odbc-connection db) con
            (qss-sql-odbc-schema db) nil
            (qss-sql-odbc-kinds db) nil
            (qss-sql-odbc-keys db) nil)
      ;; MariaDB/MySQL: "name" is an identifier only with ANSI_QUOTES
      (when (string-match-p "mysql\\|mariadb" (downcase (qss-odbc-connection-dbms con)))
        (qss-odbc-execute con "SET SESSION sql_mode = CONCAT(@@SESSION.sql_mode, ',ANSI_QUOTES')")))
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
               (setf (qss-sql-odbc-schema db) nil (qss-sql-odbc-kinds db) nil
                     (qss-sql-odbc-keys db) nil)))
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

(defun qss-sql-odbc--ascii-fold (sql)
  "SQL that folds the ASCII capitals of SQL to small letters, nothing else.
For databases without translate(): 26 nested REPLACE."
  (let ((s sql))
    (dotimes (i 26)
      (setq s (format "REPLACE(%s, '%c', '%c')" s (+ ?A i) (+ ?a i))))
    s))

(defconst qss-sql-odbc--mysql-dialect
  (list :true "(1=1)" :false "(1=0)"
        ;; MariaDB/MySQL have no IS [NOT] DISTINCT FROM
        :is "<=>" :is-not "NOT (%s <=> %s)"
        :null-last "CASE WHEN %s IS NULL THEN 1 ELSE 0 END, %s"
        :null-first-desc "CASE WHEN %s IS NULL THEN 1 ELSE 0 END DESC, %s DESC"
        ;; no translate(); the collation folds every letter, so compare
        ;; in utf8mb4_bin after folding ASCII only
        :like (format "(%s LIKE %s COLLATE utf8mb4_bin)"
                      (qss-sql-odbc--ascii-fold "CONVERT(%s USING utf8mb4)")
                      (qss-sql-odbc--ascii-fold "CONVERT(%s USING utf8mb4)"))
        ;; the bytes of UTF-8 sort as the code points
        :binary-text "CAST(%s AS BINARY)")
  "MariaDB and MySQL; identifiers in double quotes need ANSI_QUOTES, which
every connection sets (`qss-sql-odbc--connection').")

(defun qss-sql-odbc--mysql-p (db)
  (string-match-p "mysql\\|mariadb" (qss-sql-odbc--dbms db)))

(cl-defmethod qss-sql-db-dialect ((db qss-sql-odbc))
  "SQLite through ODBC speaks SQLite, MariaDB and MySQL their own; the
others standard SQL."
  (cond ((qss-sql-odbc--sqlite-p db) qss-sql--sqlite-dialect)
        ((qss-sql-odbc--mysql-p db) qss-sql-odbc--mysql-dialect)
        (t qss-sql-odbc--standard-dialect)))

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

(cl-defmethod qss-sql-db-rows ((db qss-sql-odbc) rel run keys)
  "Rows as property lists.  Rows straight from a table with a key
remember the values of the key (`qss-sql-rowid'), for editing."
  (let* ((id-cols (and (qss-sql--direct-p run)
                       (qss-sql-db-key db (qss-relation-table rel))))
         (n (length id-cols))
         (q (qss-sql-query
             run (and id-cols
                      ;; the key first, then the columns by name (MariaDB
                      ;; has no "key, *")
                      (mapconcat #'identity
                                 (append id-cols
                                         (mapcar (lambda (c) (qss-sql-quote (nth 1 c)))
                                                 (qss-relation-columns run)))
                                 ", ")))))
    (mapcar (lambda (r)
              (let ((row (cl-mapcan #'list keys (nthcdr n r))))
                (when id-cols (puthash row (take n r) qss-sql--rowids))
                row))
            (qss-sql-db-select db (car q) (cdr q)))))

(cl-defmethod qss-sql-db-key ((db qss-sql-odbc) table)
  "The primary key; PostgreSQL ctid and SQLite rowid if there is none;
MariaDB/MySQL the hidden _qss_rowid of a table made by `into'.  Nil for
a view or a table without key."
  (qss-sql-db-schema db)                ; reads the kinds too
  (let ((cached (assoc table (qss-sql-odbc-keys db))))
    (if cached
        (cdr cached)
      (let* ((kind (qss-sql-odbc--kind db table))
             (dbms (qss-sql-odbc--dbms db))
             (key (when (equal (plist-get kind :type) "TABLE")
                    (cond ((plist-get kind :rowid) (list (qss-sql-quote qss-sql-odbc--rowid)))
                          ((let ((pk (qss-odbc-primary-key (qss-sql-odbc--connection db)
                                                           (plist-get kind :table)
                                                           (plist-get kind :schema))))
                             (and pk (mapcar #'qss-sql-quote pk))))
                          ((string-match-p "postgres" dbms) (list "ctid"))
                          ((string-match-p "sqlite" dbms) (list "rowid"))))))
        (push (cons table key) (qss-sql-odbc-keys db))
        key))))

(cl-defmethod qss-sql-db-call-in-transaction ((db qss-sql-odbc) fn)
  "One transaction on the connection of DB."
  (let* ((con (qss-sql-odbc--connection db))
         (handle (qss-odbc-connection-handle con))
         (ok nil))
    (qss-odbc--set-autocommit handle nil)
    (unwind-protect
        (prog1 (funcall fn (lambda (sql params) (apply #'qss-odbc-execute con sql params)))
          (qss-odbc--commit handle)
          (setq ok t))
      (unless ok (qss-odbc--rollback handle))
      (qss-odbc--set-autocommit handle t))))

(defun qss-sql-odbc--has-rowid-p (con table &optional schema)
  "Return non-nil if TABLE (in SCHEMA) of CON has the column `qss-sql-odbc--rowid'."
  (cl-some (lambda (c) (equal (plist-get c :column_name) qss-sql-odbc--rowid))
           (qss-odbc-table-columns con table schema)))

(defun qss-sql-odbc--kind (db name)
  "The property list of the table NAME of DB (see `qss-sql-odbc-kinds')."
  (qss-sql-db-schema db)                ; reads the kinds too
  (cdr (assoc name (qss-sql-odbc-kinds db))))

(defun qss-sql-odbc--current-schema (db)
  "The current schema of DB (PostgreSQL), or nil."
  (when (string-match-p "postgres" (qss-sql-odbc--dbms db))
    (caar (cdr (qss-odbc-query (qss-sql-odbc--connection db) "SELECT current_schema()")))))

(cl-defmethod qss-sql-db-quote-table ((db qss-sql-odbc) name)
  "A table outside the current schema as \"schema\".\"table\"."
  (let ((kind (qss-sql-odbc--kind db name)))
    (if (plist-get kind :qualified)
        (concat (qss-sql-quote (plist-get kind :schema)) "." (qss-sql-quote (plist-get kind :table)))
      (qss-sql-quote name))))

(cl-defmethod qss-sql-db-order-id ((db qss-sql-odbc) table)
  "The input order of a table (not a view): PostgreSQL ctid (the order a
plain SELECT gives), SQLite rowid; MariaDB/MySQL the hidden _qss_rowid
of a table made by `into', else a primary key of one column.  Without
one, ties after sort-by have no defined order."
  (let ((kind (qss-sql-odbc--kind db table)))
    (when (equal (plist-get kind :type) "TABLE")
      (let ((dbms (qss-sql-odbc--dbms db)))
        (cond ((string-match-p "postgres" dbms) "ctid")
              ((string-match-p "sqlite" dbms) "rowid")
              ((plist-get kind :rowid) (qss-sql-quote qss-sql-odbc--rowid))
              (t (let ((pk (qss-odbc-primary-key (qss-sql-odbc--connection db)
                                                 (plist-get kind :table) (plist-get kind :schema))))
                   (and (= (length pk) 1) (qss-sql-quote (car pk))))))))))

(cl-defmethod qss-sql-db-schema ((db qss-sql-odbc))
  "Tables and views through the catalog functions, cached.
A table outside the current schema (PostgreSQL) is named SCHEMA.TABLE."
  (or (qss-sql-odbc-schema db)
      (let* ((con (qss-sql-odbc--connection db))
             (current (qss-sql-odbc--current-schema db))
             (rows (cl-remove-if
                    (lambda (r) (or (equal (plist-get r :table_name) qss-sql--types-table)
                                    (string-prefix-p "sqlite_" (plist-get r :table_name))))
                    (append (qss-odbc-tables con "TABLE") (qss-odbc-tables con "VIEW"))))
             (qss-types (qss-sql-odbc--read-types db))
             (kinds
              (mapcar (lambda (r)
                        (let* ((table (plist-get r :table_name))
                               (schema (let ((sc (plist-get r :table_schem)))
                                         (and (stringp sc) (not (string-empty-p sc)) sc)))
                               (qualified (and current schema (not (equal schema current))))
                               (name (if qualified (concat schema "." table) table)))
                          (list name :type (plist-get r :table_type) :schema schema :table table
                                :qualified qualified
                                :rowid (and (qss-sql-odbc--mysql-p db)
                                            (qss-sql-odbc--has-rowid-p con table schema)))))
                      rows))
             (tables
              (mapcar (lambda (k)
                        (let ((name (car k)))
                          (cons name
                                (delq nil
                                      (mapcar (lambda (c)
                                                (let ((col (plist-get c :column_name)))
                                                  (unless (equal col qss-sql-odbc--rowid)
                                                    (list (qss-sql-name-key col) col
                                                          (or (plist-get c :type_name) "")
                                                          (cdr (assoc (cons (downcase name) col) qss-types))))))
                                              (qss-odbc-table-columns con (plist-get (cdr k) :table)
                                                                      (plist-get (cdr k) :schema)))))))
                      kinds)))
        (dolist (tab tables) (qss-sql--check-collisions (car tab) (cdr tab)))
        (setf (qss-sql-odbc-kinds db) kinds)
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
(defun qss-odbc-source (dsn &optional table &rest options)
  "Return the table TABLE of the ODBC data source DSN as a query.
Without TABLE, the tables of DSN; RET on one shows its rows.  DSN is a
data source name or a connection string.  TABLE may be SCHEMA.TABLE for
a table outside the current schema (PostgreSQL).  OPTIONS: :user and
:password, if the DSN does not have them:

  (odbc \"test\" \"orders\" :user \"frido\" :password (read-passwd \"Password: \"))"
  (when (keywordp table)                ; (odbc DSN :user ...)
    (setq options (cons table options) table nil))
  (let ((db (apply #'qss-sql-odbc-database dsn options)))
    (if table
        (qss-sql-relation db (if (symbolp table) (symbol-name table) table))
      (qss-sql-db-tables db))))

;;; ------------------------------------------------------------------
;;; Writing: (into odbc DSN NAME [:replace | :append])

(defun qss-sql-odbc--sql-type (db type)
  "The column type in DB for TYPE, (SQLITE-TYPE . QSS-TYPE) as
`qss-sql--column-types' gives it: INTEGER, REAL or TEXT, and `time' for
times, which become TIMESTAMP (MariaDB/MySQL: DATETIME) instead of text."
  (let ((sqlite-type (car type)) (qss-type (cdr type)))
    (cond ((qss-sql-odbc--sqlite-p db) sqlite-type)
          ((eq qss-type 'time) (if (qss-sql-odbc--mysql-p db) "DATETIME" "TIMESTAMP"))
          (t (pcase sqlite-type
               ("INTEGER" "BIGINT")
               ("REAL" "DOUBLE PRECISION")
               (_ (if (qss-sql-odbc--mysql-p db) "LONGTEXT" "TEXT")))))))

(defun qss-sql-odbc-write (table db name &optional mode)
  "Write the rows of TABLE into the table NAME of the ODBC database DB.
As `qss-sql-write': MODE `replace' or `append' if NAME exists; the qss
types of the columns are recorded.  Return the number of rows written."
  (let* ((con (qss-sql-odbc--connection db))
         (specs (qss-sql--stored-specs table))
         (rows (qss-table-rows table))
         (quoted (qss-sql-quote name))
         (types-table (qss-sql-quote qss-sql--types-table))
         ;; the tables of the current schema: into writes there
         (current (qss-sql-odbc--current-schema db))
         (tables (mapcar (lambda (r) (plist-get r :table_name))
                         (seq-filter (lambda (r) (or (null current)
                                                     (equal (plist-get r :table_schem) current)))
                                     (qss-odbc-tables con "TABLE"))))
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
		(qss-odbc-execute con (format "CREATE TABLE %s (%s%s)" quoted
                                              (mapconcat (lambda (st)
							   (format "%s %s" (qss-sql-quote (symbol-name (car (car st))))
								   (qss-sql-odbc--sql-type db (cdr st))))
							 (cl-mapcar #'cons specs types)
							 ", ")
                                              ;; MariaDB/MySQL have no rowid: a hidden one
                                              (if (qss-sql-odbc--mysql-p db)
                                                  (format ", %s BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY INVISIBLE"
                                                          (qss-sql-quote qss-sql-odbc--rowid))
                                                "")))
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
