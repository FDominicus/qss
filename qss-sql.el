;;; qss-sql.el --- SQLite as a source: the words become SQL  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; Layer 3 of qss.  A table in an SQLite file is a source like files and
;; processes, with the same words:
;;
;;   (qss-> (sqlite (qss-demo-database) "Orders")
;;          (where (> :amount 100))
;;          (sort-by order-date :desc)
;;          (first 20)
;;          (select order-no order-date amount))
;;
;;   (qss-> (tables (qss-demo-database)) (where (like :name "%Order%")))
;;
;; `qss-demo-database' creates a small database to try this with.
;;
;; The pipe stays a query (a relation) until someone needs the rows
;; (display, or a word that cannot become SQL), as in dbplyr:
;;
;;   become SQL:            where (condition as data), sort-by, first,
;;                          count, select, reject (SELECT with columns), columns
;;   fetch the rows, then   where with `it' or with functions over
;;   continue in memory:    columns, last, get, each, group-by
;;
;; Values in conditions become `?' parameters, never text in the SQL.
;; The word `sql' shows the query of the pipe.
;;
;; Names are derived mechanically from the database, not translated:
;; CamelCase -> hyphen, umlauts -> ae/oe/ue/ss, `_' -> hyphen, lower
;; case:  CustomerNo -> customer-no, Straße -> strasse,
;; HT_WeighingData -> ht-weighing-data.  The way back is looked up in
;; the schema, never computed.
;;
;; Rows are property lists (:customer-no 10027 :name "...").  Display
;; fetches at most `qss-sql-display-limit' rows; `count' counts all.

;;; Code:

(require 'cl-lib)
(require 'qss-pipe)

(defcustom qss-sql-display-limit 1000
  "Maximum number of rows a query fetches for display."
  :type 'integer
  :group 'qss)

(defun qss-sql--check ()
  "Signal an error unless this Emacs has SQLite."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error "This Emacs has no SQLite (Emacs 29 built with libsqlite3 is needed)")))

;;; ------------------------------------------------------------------
;;; Names

(defconst qss-sql--umlauts
  '((?ä . "ae") (?ö . "oe") (?ü . "ue") (?Ä . "Ae") (?Ö . "Oe") (?Ü . "Ue") (?ß . "ss"))
  "German umlauts and their ASCII spelling in names.")

(defun qss-sql--upper-p (c)
  "Return non-nil if C is an upper-case letter."
  (and (/= c (downcase c)) (= c (upcase c))))

(defun qss-sql--lower-p (c)
  "Return non-nil if C is a lower-case letter."
  (and (/= c (upcase c)) (= c (downcase c))))

(defun qss-sql--digit-p (c)
  "Return non-nil if C is a digit."
  (<= ?0 c ?9))

(defun qss-sql-name-key (name)
  "Return the symbol for the database NAME (a table or a column)."
  (let* ((s (mapconcat (lambda (c) (or (alist-get c qss-sql--umlauts) (string c))) name ""))
         (n (length s))
         (camel (with-output-to-string
                  (dotimes (i n)
                    (let ((c (aref s i))
                          (prev (and (> i 0) (aref s (1- i))))
                          (next (and (< (1+ i) n) (aref s (1+ i)))))
                      (when (and prev (qss-sql--upper-p c)
                                 (or (qss-sql--lower-p prev) (qss-sql--digit-p prev)
                                     (and (qss-sql--upper-p prev) next (qss-sql--lower-p next))))
                        (princ "-"))
                      (princ (string c))))))
         (parts (split-string camel "[^A-Za-z0-9]+" t)))
    (intern (downcase (mapconcat #'identity parts "-")))))

(defun qss-sql-quote (name)
  "Return NAME as a quoted SQL identifier."
  (concat "\"" (string-replace "\"" "\"\"" name) "\""))

;;; ------------------------------------------------------------------
;;; Schema: read once per file, until the file changes

(defvar qss-sql--schemas (make-hash-table :test #'equal)
  "File -> (TIME . TABLES).
TABLES is a list of (NAME . COLUMNS), COLUMNS a list of (KEY NAME TYPE).")

(defmacro qss-sql--with-db (var file &rest body)
  "Run BODY with VAR bound to an open SQLite connection to FILE."
  (declare (indent 2))
  `(let ((,var (progn (qss-sql--check) (sqlite-open ,file))))
     (unwind-protect (progn ,@body) (sqlite-close ,var))))

(defun qss-sql--file (file)
  "Return FILE expanded; signal an error if it does not exist."
  (let ((f (expand-file-name file)))
    (unless (file-exists-p f) (user-error "No database: %s" f))
    f))

(defun qss-sql-schema (file)
  "Return the tables of FILE with their columns, cached per modification time."
  (let* ((file (qss-sql--file file))
         (mtime (file-attribute-modification-time (file-attributes file)))
         (cached (gethash file qss-sql--schemas)))
    (if (and cached (time-equal-p (car cached) mtime))
        (cdr cached)
      (let ((tables
             (qss-sql--with-db db file
               (mapcar (lambda (row)
                         (let ((name (car row)))
                           (cons name
                                 (mapcar (lambda (col)
                                           (list (qss-sql-name-key (nth 1 col)) (nth 1 col)
                                                 (or (nth 2 col) "")))
                                         (sqlite-select
                                          db (format "PRAGMA table_info(%s)" (qss-sql-quote name)))))))
                       (sqlite-select db "SELECT name FROM sqlite_master
                                          WHERE type IN ('table','view')
                                            AND name NOT LIKE 'sqlite_%' ORDER BY name")))))
        (dolist (tab tables) (qss-sql--check-collisions (car tab) (cdr tab)))
        (puthash file (cons mtime tables) qss-sql--schemas)
        tables))))

(defun qss-sql--check-collisions (table columns)
  "Signal an error if two COLUMNS of TABLE map to the same key."
  (let ((seen nil))
    (dolist (c columns)
      (let ((other (assq (car c) seen)))
        (when other
          (error "%s: columns %S and %S both map to %s"
                 table (nth 1 other) (nth 1 c) (car c)))
        (push c seen)))))

(defun qss-sql--find-table (file name)
  "Return table NAME (database name, symbol or keyword) of FILE."
  (let* ((tables (qss-sql-schema file))
         (key (and (not (stringp name)) (qss--name name))))
    (or (if key
            (cl-find key tables :key (lambda (tab) (qss-sql-name-key (car tab))))
          (or (assoc name tables)
              (cl-find name tables :key #'car :test #'string-equal-ignore-case)))
        (user-error "%s has no table %s; tables: %s"
                    (abbreviate-file-name file) name
                    (mapconcat #'car tables " ")))))

;;; ------------------------------------------------------------------
;;; The relation: a query that has not run yet

(cl-defstruct (qss-relation (:constructor qss-relation--make) (:copier nil))
  "A query.
FILE and TABLE (the database name) stay the same through all stages;
COLUMNS is the schema of the table, or the selection after select or
reject.  INNER is a query this one is a subquery over (after LIMIT).
WHERES: list of (SQL . PARAMS).  ORDER: list of (KEY DESCENDING SQL),
most important first; SQL is the quoted column name.  LIMIT: a number or nil.  PROJECTED: t after
select/reject; then the query names its columns.  TITLE: the pipe so far."
  file table columns inner wheres order limit projected title)

(defun qss-sqlite (file table)
  "Return the table TABLE of the SQLite file FILE as a query."
  (let* ((file (qss-sql--file file))
         (tab (qss-sql--find-table file table)))
    (qss-relation--make :file file :table (car tab) :columns (cdr tab)
                        :title (format "%s %s" (file-name-nondirectory file) (car tab)))))

(defun qss-sql--copy (rel stage &rest changes)
  "Copy REL with the title extended by STAGE and some slots set.
The slots to set are in CHANGES, a property list of slot keywords and values."
  (let ((new (copy-sequence rel)))
    (setf (qss-relation-title new) (format "%s | %s" (qss-relation-title rel) stage))
    (while changes
      (pcase (pop changes)
        (:inner (setf (qss-relation-inner new) (pop changes)))
        (:wheres (setf (qss-relation-wheres new) (pop changes)))
        (:order (setf (qss-relation-order new) (pop changes)))
        (:limit (setf (qss-relation-limit new) (pop changes)))
        (:columns (setf (qss-relation-columns new) (pop changes)))
        (:projected (setf (qss-relation-projected new) (pop changes)))))
    new))

(defun qss-sql--wrap (rel)
  "Wrap REL as a subquery, so the next stage can be put on top of it."
  (let ((outer (copy-sequence rel)))
    (setf (qss-relation-inner outer) rel
          (qss-relation-wheres outer) nil
          (qss-relation-order outer) nil
          (qss-relation-limit outer) nil)
    outer))

(defun qss-sql--column (rel key)
  "Return the quoted database name of column KEY of REL."
  (let ((col (assq key (qss-relation-columns rel))))
    (unless col
      (user-error "%s has no column %s; columns: %s"
                  (qss-relation-table rel) key
                  (mapconcat (lambda (c) (symbol-name (car c))) (qss-relation-columns rel) " ")))
    (qss-sql-quote (nth 1 col))))

;;; Condition -> SQL

(defun qss-sql--param (value params)
  "Append VALUE to PARAMS and return \"?\"."
  (nconc params (list (qss-sql--value value)))
  "?")

(defun qss-sql--value (v)
  "Return the Lisp value V as an SQLite value; times as text like in the database."
  (cond ((qss--time-p v) (format-time-string "%Y-%m-%d %H:%M:%S" v))
        ((eq v t) 1)
        ((and (symbolp v) v) (symbol-name v))
        (t v)))

(defun qss-sql--condition (rel template values)
  "Return (SQL . PARAMS) for TEMPLATE with VALUES (from a `qss-condition').
Column names are looked up in REL."
  (let ((params (list nil)))              ; head, so that nconc appends
    (cl-labels
        ((value-of (form) (nth (cadr form) values))
         (render (f)
           (cond
            ((keywordp f) (qss-sql--column rel (qss--name f)))
            ((eq (car-safe f) 'qss-param) (qss-sql--param (value-of f) params))
            (t
             (pcase (car f)
               ('and (if (cdr f)
                         (concat "(" (mapconcat #'render (cdr f) " AND ") ")")
                       "1"))
               ('or (if (cdr f)
                        (concat "(" (mapconcat #'render (cdr f) " OR ") ")")
                      "0"))
               ('not (format "(NOT %s)" (render (cadr f))))
               ('is-null (format "(%s IS NULL)" (render (cadr f))))
               ('not-null (format "(%s IS NOT NULL)" (render (cadr f))))
               ('like (format "(%s LIKE %s)" (render (nth 1 f)) (render (nth 2 f))))
               ('in (let ((left (render (nth 1 f)))
                          (list (if (eq (car-safe (nth 2 f)) 'qss-param)
                                    (value-of (nth 2 f))
                                  (user-error "Expected a list for `in', not %S" (nth 2 f)))))
                      (if (null list)
                          "0"
                        (format "(%s IN (%s))" left
                                (mapconcat (lambda (v) (qss-sql--param v params)) list ", ")))))
               (_ (let ((op (pcase (car f)
                              ('= "=") ('/= "<>") ('< "<") ('> ">") ('<= "<=") ('>= ">="))))
                    (unless op (error "Unknown operator in qss-sql: %S" (car f)))
                    (format "(%s %s %s)" (render (nth 1 f)) op (render (nth 2 f))))))))))
      (let ((sql (render template)))
        (cons sql (cdr params))))))

;;; Relation -> SQL

(defun qss-sql-query (rel)
  "Return (SQL . PARAMS) of the query REL."
  (let* ((inner (and (qss-relation-inner rel) (qss-sql-query (qss-relation-inner rel))))
         (from (if inner
                   (format "(\n%s\n) AS t" (replace-regexp-in-string "^" "  " (car inner)))
                 (qss-sql-quote (qss-relation-table rel))))
         (wheres (qss-relation-wheres rel))
         (order (qss-relation-order rel))
         (limit (qss-relation-limit rel)))
    (cons (concat "SELECT "
                  (if (qss-relation-projected rel)
                      (mapconcat (lambda (c) (qss-sql-quote (nth 1 c)))
                                 (qss-relation-columns rel) ", ")
                    "*")
                  "\nFROM " from
                  (when wheres
                    (concat "\nWHERE " (mapconcat #'car wheres "\n  AND ")))
                  (when order
                    ;; NULL always last, as in memory
                    (concat "\nORDER BY "
                            (mapconcat (lambda (o)
                                         (let ((c (nth 2 o)))
                                           (format "%s IS NULL, %s%s" c c (if (nth 1 o) " DESC" ""))))
                                       order ", ")))
                  (when limit "\nLIMIT ?"))
          (append (cdr inner)
                  (apply #'append (mapcar #'cdr wheres))
                  (and limit (list limit))))))

(defun qss-sql--run (rel &optional limit)
  "Return the rows of REL as property lists; with LIMIT at most that many."
  (let* ((q (qss-sql-query (if limit
                               (if (and (qss-relation-limit rel) (<= (qss-relation-limit rel) limit))
                                   rel
                                 (qss-sql--copy (if (qss-relation-limit rel) (qss-sql--wrap rel) rel)
                                                "" :limit limit))
                             rel)))
         (keys (mapcar (lambda (c) (intern (format ":%s" (car c))))
                       (qss-relation-columns rel))))
    (qss-sql--with-db db (qss-relation-file rel)
      (mapcar (lambda (row) (cl-mapcan #'list keys row))
              (sqlite-select db (car q) (vconcat (cdr q)))))))

(defun qss-sql--specs (rel rows)
  "Return the column specs of REL for ROWS.
A column is a number column if all its values are numbers."
  (mapcar (lambda (key)
            (let ((kw (intern (format ":%s" key))))
              (if (and rows
                       (cl-some (lambda (r) (plist-get r kw)) rows)
                       (cl-every (lambda (r) (let ((v (plist-get r kw))) (or (null v) (numberp v))))
                                 rows))
                  (list key :type 'number)
                key)))
          (mapcar #'car (qss-relation-columns rel))))

(defun qss-sql--table (rel rows title)
  "Return a table called TITLE with ROWS of REL; its source is REL."
  (qss-make-table title rows (qss-sql--specs rel rows)
                  (lambda () (qss-as-table rel))))

;;; ------------------------------------------------------------------
;;; Methods: what can become SQL

(cl-defmethod qss-as-input ((rel qss-relation))
  "Return the query REL unchanged, so that later words can become SQL."
  rel)

(cl-defmethod qss-as-table ((rel qss-relation))
  "Fetch all rows of REL."
  (qss-sql--table rel (qss-sql--run rel) (qss-relation-title rel)))

(cl-defmethod qss-display ((rel qss-relation))
  "Show at most `qss-sql-display-limit' rows of REL."
  (let* ((rows (qss-sql--run rel (1+ qss-sql-display-limit)))
         (more (> (length rows) qss-sql-display-limit))
         (rows (if more (take qss-sql-display-limit rows) rows))
         (table (qss-sql--table
                 rel rows
                 (if more
                     (format "%s  [first %d of %d]" (qss-relation-title rel)
                             qss-sql-display-limit (qss-count rel))
                   (qss-relation-title rel)))))
    (setf (qss-table-source table)
          (lambda () (qss-sql--table rel (qss-sql--run rel qss-sql-display-limit)
                                     (qss-table-title table))))
    (qss-show table)))

(cl-defmethod qss-where ((rel qss-relation) cond)
  "Add COND to the query REL as SQL, or filter in memory if it cannot be SQL."
  (if (and (qss-condition-p cond) (qss-condition-template cond))
      (let* ((base (if (qss-relation-limit rel) (qss-sql--wrap rel) rel))
             (sql (qss-sql--condition base (qss-condition-template cond)
                                      (qss-condition-values cond))))
        (qss-sql--copy base (format "where %S" (qss-condition-form cond))
                       :wheres (append (qss-relation-wheres base) (list sql))))
    (cl-call-next-method)))

(cl-defmethod qss-sort-by ((rel qss-relation) col &optional dir)
  "Sort the query REL by COL in SQL; DIR as in the generic function."
  (let* ((key (qss--name col))
         (desc (qss--descending-p dir))
         (base (if (qss-relation-limit rel) (qss-sql--wrap rel) rel)))
    ;; The column is looked up now, not when the SQL is built: a later
    ;; select may drop it, and ORDER BY may still name it.
    (qss-sql--copy base (format "sort-by %s%s" key (if desc " desc" ""))
                   ;; the new order first, the old one breaks ties
                   :order (cons (list key desc (qss-sql--column base key))
                                (cl-remove key (qss-relation-order base) :key #'car)))))

(cl-defmethod qss-first ((rel qss-relation) &optional n)
  "Limit the query REL to N rows in SQL."
  (let ((n (or n 1)))
    (qss-sql--copy rel (format "first %d" n)
                   :limit (if (qss-relation-limit rel) (min n (qss-relation-limit rel)) n))))

(cl-defmethod qss-count ((rel qss-relation))
  "Count the rows of the query REL in SQL."
  (let ((q (qss-sql-query rel)))
    (qss-sql--with-db db (qss-relation-file rel)
      (caar (sqlite-select db (format "SELECT COUNT(*) FROM (\n%s\n) AS t" (car q))
                           (vconcat (cdr q)))))))

(defun qss-sql--project (rel keys stage)
  "Return REL cut down to the columns KEYS (in this order), titled STAGE."
  (dolist (k keys) (qss-sql--column rel k))   ; do they exist?
  (qss-sql--copy rel stage
                 :columns (mapcar (lambda (k) (assq k (qss-relation-columns rel))) keys)
                 :projected t))

(cl-defmethod qss-select ((rel qss-relation) &rest cols)
  "Select the columns COLS in the SQL of REL."
  (let ((keys (mapcar #'qss--name cols)))
    (qss-sql--project rel keys (format "select %s" (mapconcat #'symbol-name keys " ")))))

(cl-defmethod qss-reject ((rel qss-relation) &rest cols)
  "Select all columns of REL except COLS, in SQL."
  (let ((keys (mapcar #'qss--name cols)))
    (dolist (k keys) (qss-sql--column rel k))
    (qss-sql--project rel
                      (cl-remove-if (lambda (k) (memq k keys))
                                    (mapcar #'car (qss-relation-columns rel)))
                      (format "reject %s" (mapconcat #'symbol-name keys " ")))))

(cl-defmethod qss-columns-of ((rel qss-relation))
  "Return the columns of REL from the schema, without fetching rows."
  (qss-make-table (format "%s | columns" (qss-relation-title rel))
                  (mapcar (lambda (c) (list :column (nth 0 c) :name (nth 1 c) :type (nth 2 c)))
                          (qss-relation-columns rel))))

(defun qss-sql (rel)
  "Return the query REL as text with its parameters."
  (unless (qss-relation-p rel)
    (user-error "The pipe is no longer a query at this point"))
  (let ((q (qss-sql-query rel)))
    (format "%s\nParameters: %S" (car q) (cdr q))))

;;; ------------------------------------------------------------------
;;; Source: the tables of a database

(cl-defstruct (qss-db-table (:constructor qss-db-table--make) (:copier nil))
  "A table of the SQLite file FILE: its NAME and COLUMNS."
  file name columns)

(defun qss-tables (file)
  "Return the tables of the SQLite file FILE.
RET on one of them shows its rows."
  (let ((file (qss-sql--file file)))
    (qss-make-table (format "tables %s" (file-name-nondirectory file))
                    (mapcar (lambda (tab) (qss-db-table--make :file file :name (car tab)
                                                              :columns (cdr tab)))
                            (qss-sql-schema file))
                    nil
                    (lambda () (qss-tables file)))))

(cl-defmethod qss-columns ((_row qss-db-table))
  "Return the columns of a database table row."
  '((name :width 40) key (columns :type number)))

(cl-defmethod qss-field ((row qss-db-table) name)
  "Return field NAME of the database table ROW."
  (pcase name
    ('name (qss-db-table-name row))
    ('key (qss-sql-name-key (qss-db-table-name row)))
    ('columns (length (qss-db-table-columns row)))
    ('file (qss-db-table-file row))))

(cl-defmethod qss-default-action ((row qss-db-table))
  "Show the rows of the database table ROW."
  (qss-display (qss-sqlite (qss-db-table-file row) (qss-db-table-name row))))

(cl-defmethod qss-actions ((row qss-db-table))
  "Return the actions for the database table ROW."
  (ignore row)
  (list (cons "Show rows" #'qss-default-action)
        (cons "Show columns"
              (lambda (r) (qss-show (qss-columns-of (qss-sqlite (qss-db-table-file r)
                                                                (qss-db-table-name r))))))
        (cons "Count" (lambda (r) (message "%s: %d rows" (qss-db-table-name r)
                                           (qss-count (qss-sqlite (qss-db-table-file r)
                                                                  (qss-db-table-name r))))))))

;;; ------------------------------------------------------------------
;;; A demo database, to try qss without a database of your own

;;;###autoload
(defun qss-demo-database ()
  "Return the file of a small demo database, creating it if needed.
The file is qss-demo.sqlite3 in the variable `temporary-file-directory'.
Tables: Customers (CustomerNo, Name, City, Since) and Orders (OrderNo,
CustomerNo, OrderDate, Amount, Status); Status is NULL for open orders.
Try:  (qss-> (tables (qss-demo-database)))"
  (let ((file (expand-file-name "qss-demo.sqlite3" temporary-file-directory)))
    (unless (file-exists-p file)
      (qss-sql--with-db db file
        (sqlite-execute db "CREATE TABLE \"Customers\" (\"CustomerNo\" INTEGER PRIMARY KEY,
          \"Name\" TEXT, \"City\" TEXT, \"Since\" TEXT)")
        (dolist (row '((1 "Anna Meier" "Bruchsal" "2019-03-01")
                       (2 "Bob Stone" "London" "2021-07-15")
                       (3 "Clara Weiss" "Bruchsal" "2024-01-10")
                       (4 "Dan Brown" nil "2025-05-05")))
          (sqlite-execute db "INSERT INTO \"Customers\" VALUES (?,?,?,?)" (vconcat row)))
        (sqlite-execute db "CREATE TABLE \"Orders\" (\"OrderNo\" INTEGER PRIMARY KEY,
          \"CustomerNo\" INTEGER, \"OrderDate\" TEXT, \"Amount\" REAL, \"Status\" TEXT)")
        (dolist (row '((4711 1 "2026-09-01" 120.0 "paid")
                       (4712 2 "2026-09-03" 80.5 "paid")
                       (4713 1 "2026-09-20" 300.0 nil)
                       (4714 3 "2026-10-02" 45.0 "paid")
                       (4715 4 "2026-10-05" 999.99 nil)
                       (4716 3 "2026-10-12" 150.0 "cancelled")))
          (sqlite-execute db "INSERT INTO \"Orders\" VALUES (?,?,?,?,?)" (vconcat row)))))
    file))

;;; ------------------------------------------------------------------
;;; Registering with qss->

(setf (alist-get 'sqlite qss-sources) 'qss-sqlite)
(setf (alist-get 'tables qss-sources) 'qss-tables)
(setf (alist-get 'sql qss--words) 'none)

(provide 'qss-sql)
;;; qss-sql.el ends here
