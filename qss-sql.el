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
;;   continue in memory:    columns, last, get, each, group-sum
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
;;
;; The other way: (into sqlite "Name") stores the table of a pipe in
;; `qss-database' (or :file FILE) and returns it as a query, so the
;; words go on there.  An existing table needs :replace or :append.
;; Sizes and durations are stored as INTEGER (bytes, nanoseconds),
;; times as text in `qss-time-format'; the qss types are recorded in
;; the table _qss_columns, so sizes show as 9.8k again.  A file row
;; keeps its path, so RET on the stored row opens the file.
;;
;;   (qss-> (files "~") (where (> :size "10mb")) (into sqlite "big-files"))
;;   (qss-> (sqlite "big-files") (sort-by size :desc))     ; from qss-database

;;; Code:

(require 'cl-lib)
(require 'sqlite)                       ; with-sqlite-transaction
(require 'qss-pipe)

(defcustom qss-data-directory nil
  "Directory of the SQLite files of qss; nil means `user-emacs-directory'.
`qss-database' and `qss-elisp-xref-database' lie here unless they are
set themselves.  Choose a local disk when `user-emacs-directory' is on a
network drive shared by several Emacs: SQLite must not be used over
SMB or NFS by two processes at once (its locking is unreliable there)."
  :type '(choice (const :tag "user-emacs-directory" nil) directory)
  :group 'qss)

(defcustom qss-database nil
  "The central SQLite file of qss; nil means qss.sqlite3 in `qss-data-directory'.
\=(into sqlite \"Name\") stores there, (sqlite \"Name\") and (tables)
read from there, unless a file is given."
  :type '(choice (const :tag "qss.sqlite3 in qss-data-directory" nil) file)
  :group 'qss)

(defun qss-data-file (name)
  "The file NAME in `qss-data-directory', as an absolute file name."
  (expand-file-name name (or qss-data-directory user-emacs-directory)))

(defun qss-database-file ()
  "The central SQLite file: `qss-database', or qss.sqlite3 in `qss-data-directory'."
  (expand-file-name (or qss-database (qss-data-file "qss.sqlite3"))))

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
TABLES is a list of (NAME . COLUMNS), COLUMNS a list of (KEY NAME TYPE
QSS-TYPE).  QSS-TYPE is the column type of qss (`size', `duration',
`number', `time') recorded by `into', or nil.")

(defconst qss-sql--types-table "_qss_columns"
  "Table in which `into' records the qss types of the columns it writes.
Not shown by `tables'.")

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

;;; ------------------------------------------------------------------
;;; The database behind a query
;;
;; A query (`qss-relation') keeps its database in the slot FILE: a file
;; name means SQLite, as always.  Other databases (qss-sql-odbc.el) add
;; methods for these generic functions.  For a file name they do what
;; this file did before, and the SQL text stays the same.

(cl-defgeneric qss-sql-db-select (db sql params)
  "Run the query SQL with PARAMS (a list) in DB; return the rows as lists.")

(cl-defgeneric qss-sql-db-schema (db)
  "Return the tables of DB: a list of (NAME . COLUMNS) as `qss-sql-schema'.")

(cl-defgeneric qss-sql-db-label (db)
  "Return DB as a short name for titles and messages.")

(cl-defgeneric qss-sql-db-order-id (db table)
  "Return the SQL for the order in which TABLE of DB has its rows, or nil.
SQLite: rowid, for a table (not a view).  It breaks the last ties of
sort-by, as the input order does in memory.")

(cl-defgeneric qss-sql-db-dialect (db)
  "Return the SQL dialect of DB, a property list.
:true :false   conditions that are always true or false
:is :is-not    = and /= that treat NULL as a value: an operator (\"IS\"),
               or a format with two %s (\"NOT (%s <=> %s)\")
:null-last     format for ORDER BY ascending with NULL last (column twice)
:null-first-desc  the same descending, NULL first
:like          format for (LEFT LIKE PATTERN), case folded in ASCII only
:binary-text   format that makes a text column compare by code points,
               as SQLite and memory do, or nil if it does already
:avg           format for the argument of AVG, or nil: MariaDB/MySQL
               round the average of DECIMAL to 4 more digits")

(cl-defgeneric qss-sql-db-value (db value)
  "Return the Lisp VALUE as a parameter for DB.")

(cl-defgeneric qss-sql-db-rows (db rel run keys)
  "Return the rows of the query RUN (made from REL) in DB as property lists.
KEYS are the keywords of the columns of REL.")

(cl-defgeneric qss-sql-db-key (db table)
  "Return the SQL of the columns that identify a row of TABLE in DB, a list.
Nil if rows cannot be told apart (a view, a table without key).
SQLite: (\"rowid\").  Editing (qss-edit.el) finds rows by it.")

(cl-defgeneric qss-sql-db-call-in-transaction (db fn)
  "Call FN with one argument, a function (SQL PARAMS) -> rows changed,
all in one transaction of DB: committed if FN returns, rolled back if it
exits otherwise.  Return what FN returns.")

(cl-defmethod qss-sql-db-key ((db string) table)
  "SQLite: rowid of a real table."
  (let ((id (qss-sql-db-order-id db table)))
    (and id (list id))))

(cl-defmethod qss-sql-db-call-in-transaction ((db string) fn)
  "SQLite: the file DB."
  (qss-sql--with-db d db
    (with-sqlite-transaction d
      (funcall fn (lambda (sql params) (sqlite-execute d sql (vconcat params)))))))

(cl-defgeneric qss-sql-db-quote-table (db name)
  "Return the table NAME of DB quoted for SQL.
SQLite: one identifier.  Other databases may know NAME as schema.table.")

(cl-defmethod qss-sql-db-quote-table ((_db string) name)
  "SQLite: NAME is one identifier, dots and all."
  (qss-sql-quote name))

(defun qss-sql--quote-table (rel)
  "The table of the query REL, quoted for SQL."
  (qss-sql-db-quote-table (qss-relation-file rel) (qss-relation-table rel)))

(defconst qss-sql--sqlite-dialect
  '(:true "1" :false "0" :is "IS" :is-not "IS NOT"
    :null-last "%s IS NULL, %s" :null-first-desc "%s IS NULL DESC, %s DESC"
    :like "(%s LIKE %s)")
  "The dialect of SQLite: LIKE folds ASCII only, IS compares NULL.")

(cl-defmethod qss-sql-db-dialect ((_db string))
  "SQLite."
  qss-sql--sqlite-dialect)

(cl-defmethod qss-sql-db-select ((db string) sql params)
  "SQLite: the file DB."
  (qss-sql--with-db d db
    (sqlite-select d sql (vconcat params))))

(cl-defmethod qss-sql-db-schema ((db string))
  "SQLite: the file DB."
  (qss-sql-schema db))

(cl-defmethod qss-sql-db-label ((db string))
  "SQLite: the file name without directory."
  (file-name-nondirectory db))

(cl-defmethod qss-sql-db-order-id ((db string) table)
  "SQLite: rowid of a real table."
  (qss-sql--with-db d db
    (and (equal (caar (sqlite-select d "SELECT type FROM sqlite_master WHERE name = ? COLLATE NOCASE"
                                     (vector table)))
                "table")
         "rowid")))

(cl-defmethod qss-sql-db-value ((_db string) value)
  "SQLite: times as text, t as 1, symbols by name."
  (qss-sql--value value))

(defun qss-sql--dialect (rel key)
  "Return KEY of the dialect of the database of REL."
  (plist-get (qss-sql-db-dialect (qss-relation-file rel)) key))

(defun qss-sql--text-column-p (type)
  "Return non-nil if the declared column TYPE holds text."
  (and type (eq (qss-sql--affinity type) 'text)))

(defun qss-sql--binary (rel sql type)
  "SQL of a column of TYPE in REL, made to compare by code points.
The database may sort text by a language (PostgreSQL with de_DE puts
\"Ärger\" after \"Anna\" and \"bert\" before \"Carla\"); memory and
SQLite compare code points."
  (let ((fmt (qss-sql--dialect rel :binary-text)))
    (if (and fmt (qss-sql--text-column-p type)) (format fmt sql) sql)))

(defun qss-sql--same-db-p (a b)
  "Return non-nil if A and B are the same database."
  (if (and (stringp a) (stringp b)) (equal a b) (eq a b)))

(defun qss-sql-schema (file)
  "Return the tables of FILE with their columns, cached per modification time."
  (let* ((file (qss-sql--file file))
         (mtime (file-attribute-modification-time (file-attributes file)))
         (cached (gethash file qss-sql--schemas)))
    (if (and cached (time-equal-p (car cached) mtime))
        (cdr cached)
      (let ((tables
             (qss-sql--with-db db file
               (let ((qss-types (qss-sql--read-types db)))
                 (mapcar (lambda (row)
                           (let ((name (car row)))
                             (cons name
                                   (mapcar (lambda (col)
                                             (list (qss-sql-name-key (nth 1 col)) (nth 1 col)
                                                   (or (nth 2 col) "")
                                                   (cdr (assoc (cons (downcase name) (nth 1 col))
                                                               qss-types))))
                                           (sqlite-select
                                            db (format "PRAGMA table_info(%s)" (qss-sql-quote name)))))))
                         (sqlite-select db "SELECT name FROM sqlite_master
                                            WHERE type IN ('table','view')
                                              AND name NOT LIKE 'sqlite_%'
                                              AND substr(name, 1, 5) <> '_qss_'
                                            ORDER BY name"))))))
        (dolist (tab tables) (qss-sql--check-collisions (car tab) (cdr tab)))
        (puthash file (cons mtime tables) qss-sql--schemas)
        tables))))

(defun qss-sql--read-types (db)
  "Return the qss types recorded in DB: ((TABLE . COLUMN) . TYPE) ...
TABLE is in lower case: SQLite does not distinguish case in names."
  (when (sqlite-select db "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?"
                       (vector qss-sql--types-table))
    (mapcar (lambda (row) (cons (cons (downcase (nth 0 row)) (nth 1 row)) (intern (nth 2 row))))
            (sqlite-select db (format "SELECT \"table\", \"column\", \"type\" FROM %s"
                                      (qss-sql-quote qss-sql--types-table))))))

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
  "Return table NAME (database name, symbol or keyword) of FILE.
FILE is a database: an SQLite file, or another one (`qss-sql-db-schema')."
  (let* ((tables (qss-sql-db-schema file))
         (key (and (not (stringp name)) (qss--name name))))
    (or (if key
            (cl-find key tables :key (lambda (tab) (qss-sql-name-key (car tab))))
          (or (assoc name tables)
              (cl-find name tables :key #'car :test #'string-equal-ignore-case)))
        (user-error "%s has no table %s; tables: %s"
                    (if (stringp file) (abbreviate-file-name file) (qss-sql-db-label file)) name
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
select/reject; then the query names its columns.  TITLE: the pipe so far.
SOURCE: (SQL . PARAMS) of a query this one reads from instead of TABLE
\(after join and group-sum); its columns are named by their keys.
ROW-FN: a function that turns each fetched row into the row object
\(groups), or nil."
  file table columns inner wheres order limit projected title source row-fn)

(defun qss-sqlite (file &optional table)
  "Return the table TABLE of the SQLite file FILE as a query.
With one argument, FILE is the name of a table in `qss-database'."
  (unless table
    (when (string-match-p "[/\\]\\|\\.\\(?:sqlite3?\\|db\\)\\'" file)
      (user-error "%s looks like a file: give the table too, or use (tables %S)" file file))
    (setq table file
          file (qss-database-file)))
  (qss-sql-relation (qss-sql--file file) table))

(defun qss-sql-relation (db table)
  "Return the table TABLE of the database DB as a query.
DB is an SQLite file name or another database (see qss-sql-odbc.el)."
  (let ((tab (qss-sql--find-table db table)))
    (qss-relation--make :file db :table (car tab) :columns (cdr tab)
                        :title (format "%s %s" (qss-sql-db-label db) (car tab)))))

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
        (:projected (setf (qss-relation-projected new) (pop changes)))
        (:source (setf (qss-relation-source new) (pop changes)))
        (:row-fn (setf (qss-relation-row-fn new) (pop changes)))))
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

(defun qss-sql--param (value params &optional db)
  "Append VALUE to PARAMS and return \"?\".
VALUE becomes a parameter of the database DB (default: SQLite)."
  (nconc params (list (if db (qss-sql-db-value db value) (qss-sql--value value))))
  "?")

(defun qss-sql--value (v)
  "Return the Lisp value V as an SQLite value; times as text like in the database."
  (cond ((qss--time-p v) (format-time-string qss-time-format v))
        ((eq v t) 1)
        ((and (symbolp v) v) (symbol-name v))
        (t v)))

(defun qss-sql--affinity (type)
  "Return the SQLite affinity of the declared column TYPE.
`numeric' (INTEGER, REAL, NUMERIC ...), `text' or `none' (BLOB or no
type), by the rules of SQLite."
  (let ((ty (upcase type)))
    (cond ((string-match-p "INT" ty) 'numeric)
          ((string-match-p "CHAR\\|CLOB\\|TEXT" ty) 'text)
          ((or (string-empty-p ty) (string-match-p "BLOB" ty)) 'none)
          (t 'numeric))))

(defun qss-sql--quantities-p (rel op operands)
  "Return non-nil if sizes and durations as text become numbers for OP.
OPERANDS are those of the comparison in the template.  For < > <= >=
on any column without text type; for = /= and `in' only on a column of
numeric type.  So (> :size \"10kb\") means 10000 bytes, as in memory,
and \"10kb\" in a text column stays text."
  (let* ((col (cl-find-if #'keywordp operands))
         (type (and col (nth 2 (assq (qss--name col) (qss-relation-columns rel)))))
         (affinity (and type (qss-sql--affinity type))))
    (and affinity
         (if (memq op '(< > <= >=))
             (not (eq affinity 'text))
           (eq affinity 'numeric)))))

(defun qss-sql--quantity (v)
  "Return V, or its number if V is a size or duration as text (\"10kb\")."
  (let ((q (and (stringp v) (qss-parse-quantity v))))
    (if q (cdr q) v)))

(defun qss-sql--condition (rel template values)
  "Return (SQL . PARAMS) for TEMPLATE with VALUES (from a `qss-condition').
Column names are looked up in REL.  Sizes and durations as text become
numbers where `qss-sql--quantities-p' says so."
  (let ((params (list nil))               ; head, so that nconc appends
        (quantities nil)                  ; inside a comparison that converts
        (db (qss-relation-file rel))
        (dialect (qss-sql-db-dialect (qss-relation-file rel))))
    (cl-labels
        ((value-of (form)
           (let ((v (nth (cadr form) values)))
             (cond ((not quantities) v)
                   ((consp v) (mapcar #'qss-sql--quantity v)) ; the list of `in'
                   (t (qss-sql--quantity v)))))
         (compare (f)
           (let ((outer quantities))
             (setq quantities (qss-sql--quantities-p rel (car f) (cdr f)))
             (unwind-protect (render-1 f)
               (setq quantities outer))))
         (render (f)
           (if (memq (car-safe f) '(= /= < > <= >= in))
               (compare f)
             (render-1 f)))
         (render-1 (f)
           (cond
            ((keywordp f) (qss-sql--column rel (qss--name f)))
            ((eq (car-safe f) 'qss-param)
             (let ((v (value-of f)))
               (qss-sql--param v params db)))
            (t
             (pcase (car f)
               ('and (if (cdr f)
                         (concat "(" (mapconcat #'render (cdr f) " AND ") ")")
                       (plist-get dialect :true)))
               ('or (if (cdr f)
                        (concat "(" (mapconcat #'render (cdr f) " OR ") ")")
                      (plist-get dialect :false)))
               ('not (format "(NOT %s)" (render (cadr f))))
               ('is-null (format "(%s IS NULL)" (render (cadr f))))
               ('not-null (format "(%s IS NOT NULL)" (render (cadr f))))
               ('like (format (plist-get dialect :like) (render (nth 1 f)) (render (nth 2 f))))
               ('in (let ((left (render (nth 1 f)))
                          (list (if (eq (car-safe (nth 2 f)) 'qss-param)
                                    (value-of (nth 2 f))
                                  (user-error "Expected a list for `in', not %S" (nth 2 f)))))
                      ;; NULL is in the list only if the list has nil, as in Nushell
                      (let* ((null-in (memq nil list))
                             (vals (remq nil list))
                             (in (and vals (format "%s IN (%s)" left
                                                   (mapconcat (lambda (v) (qss-sql--param v params db))
                                                              vals ", ")))))
                        (cond ((and null-in in) (format "(%s IS NULL OR %s)" left in))
                              (null-in (format "(%s IS NULL)" left))
                              (in (format "(%s IS NOT NULL AND %s)" left in))
                              (t (plist-get dialect :false))))))
               ;; = and /= treat NULL as a value, as Nushell: IS and IS NOT
               (_ (let ((op (pcase (car f)
                              ('= (plist-get dialect :is)) ('/= (plist-get dialect :is-not))
                              ('< "<") ('> ">") ('<= "<=") ('>= ">="))))
                    (unless op (error "Unknown operator in qss-sql: %S" (car f)))
                    (let ((left (render (nth 1 f))))
                      ;; < > on text by code points, as in memory
                      (when (and (memq (car f) '(< > <= >=)) (keywordp (nth 1 f)))
                        (setq left (qss-sql--binary
                                    rel left (nth 2 (assq (qss--name (nth 1 f))
                                                          (qss-relation-columns rel))))))
                      (if (string-search "%s" op)
                          (let ((right (render (nth 2 f))))
                            (concat "(" (format op left right) ")"))
                        (format "(%s %s %s)" left op (render (nth 2 f))))))))))))
      (let ((sql (render template)))
        (cons sql (cdr params))))))

;;; Relation -> SQL

(defun qss-sql-query (rel &optional select)
  "Return (SQL . PARAMS) of the query REL.
SELECT, if given, replaces the list of columns (e.g. \"rowid\")."
  (let* ((inner (cond ((qss-relation-inner rel) (qss-sql-query (qss-relation-inner rel)))
                      ((qss-relation-source rel))))
         (from (if inner
                   (format "(\n%s\n) AS t" (replace-regexp-in-string "^" "  " (car inner)))
                 (qss-sql--quote-table rel)))
         (wheres (qss-relation-wheres rel))
         (order (qss-relation-order rel))
         (limit (qss-relation-limit rel))
         (dialect (qss-sql-db-dialect (qss-relation-file rel))))
    (cons (concat "SELECT "
                  (cond
                   (select select)
                   ((qss-relation-projected rel)
                    (mapconcat (lambda (c) (qss-sql-quote (nth 1 c)))
                               (qss-relation-columns rel) ", "))
                   (t "*"))
                  "\nFROM " from
                  (when wheres
                    (concat "\nWHERE " (mapconcat #'car wheres "\n  AND ")))
                  (when order
                    ;; NULL last ascending, first descending (reversed), as in memory
                    (concat "\nORDER BY "
                            (mapconcat (lambda (o)
                                         (let ((c (nth 2 o)))
                                           (cond ((eq (car o) 'qss--rowid)
                                                  (if (nth 1 o) (format "%s DESC" c) c))
                                                 ((nth 1 o) (format (plist-get dialect :null-first-desc) c c))
                                                 (t (format (plist-get dialect :null-last) c c)))))
                                       order ", ")))
                  (when limit "\nLIMIT ?"))
          (append (cdr inner)
                  (apply #'append (mapcar #'cdr wheres))
                  (and limit (list limit))))))

(defvar qss-sql--rowids (make-hash-table :test #'eq :weakness 'key)
  "Row -> its rowid, for rows fetched straight from a table.
Editing (qss-edit.el) and `qss-sql-rowid' use it.")

(defun qss-sql-rowid (row)
  "Return what identifies ROW in its table, or nil.
For a row of an SQLite table its rowid; for other databases the list of
the values of the key (`qss-sql-db-key'): the primary key, ctid ..."
  (gethash row qss-sql--rowids))

(defun qss-sql--direct-p (rel)
  "Return non-nil if REL reads straight from its table (no subquery)."
  (not (or (qss-relation-inner rel) (qss-relation-source rel))))

(defun qss-sql--real-table-p (db rel)
  "Return non-nil if the table of REL is a table in DB (not a view)."
  (equal (caar (sqlite-select db "SELECT type FROM sqlite_master WHERE name = ? COLLATE NOCASE"
                              (vector (qss-relation-table rel))))
         "table"))

(defun qss-sql--run (rel &optional limit)
  "Return the rows of REL as property lists; with LIMIT at most that many.
Rows straight from a table remember their rowid (`qss-sql-rowid')."
  (let* ((run (if limit
                  (if (and (qss-relation-limit rel) (<= (qss-relation-limit rel) limit))
                      rel
                    (qss-sql--copy (if (qss-relation-limit rel) (qss-sql--wrap rel) rel)
                                   "" :limit limit))
                rel))
         (keys (mapcar (lambda (c) (intern (format ":%s" (car c))))
                       (qss-relation-columns rel)))
         (rows (qss-sql-db-rows (qss-relation-file rel) rel run keys)))
    (if (qss-relation-row-fn rel)
        (mapcar (qss-relation-row-fn rel) rows)
      rows)))

(cl-defmethod qss-sql-db-rows ((file string) rel run keys)
  "SQLite: rows straight from a table remember their rowid."
    (qss-sql--with-db db file
      (let* ((with-rowid (and (qss-sql--direct-p run) (qss-sql--real-table-p db rel)))
             (q (qss-sql-query run (and with-rowid
                                        (concat "rowid, "
                                                (if (qss-relation-projected run)
                                                    (mapconcat (lambda (c) (qss-sql-quote (nth 1 c)))
                                                               (qss-relation-columns run) ", ")
                                                  "*")))))
             (raw (condition-case nil
                      (sqlite-select db (car q) (vconcat (cdr q)))
                    ;; a table WITHOUT ROWID: no rowids, no editing
                    (error (setq with-rowid nil)
                           (let ((q (qss-sql-query run)))
                             (sqlite-select db (car q) (vconcat (cdr q)))))))
             (rows (mapcar (lambda (r)
                             (let ((row (cl-mapcan #'list keys (if with-rowid (cdr r) r))))
                               (when with-rowid (puthash row (car r) qss-sql--rowids))
                               row))
                           raw)))
        rows)))

(defun qss-sql--specs (rel rows)
  "Return the column specs of REL for ROWS.
A column written by `into' as size, duration or number has that type
again; otherwise a column is a number column if all its values are
numbers.  Times are stored as text and stay text."
  (mapcar (lambda (col)
            (let* ((key (car col))
                   (qss-type (nth 3 col))
                   (kw (intern (format ":%s" key))))
              (ignore kw)
              (cond ((memq qss-type '(size duration number)) (list key :type qss-type))
                    ((and rows
                          (cl-some (lambda (r) (qss-field r key)) rows)
                          (cl-every (lambda (r) (let ((v (qss-field r key))) (or (null v) (numberp v))))
                                    rows))
                     (list key :type 'number))
                    (t key))))
          (qss-relation-columns rel)))

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
    (qss-show table rel)))

(cl-defmethod qss-where ((rel qss-relation) cond)
  "Add COND to the query REL as SQL, or filter in memory if it cannot be SQL."
  (if (and (qss-condition-p cond) (qss-condition-template cond))
      (let* ((base (if (qss-relation-limit rel) (qss-sql--wrap rel) rel))
             (sql (qss-sql--condition base (qss-condition-template cond)
                                      (qss-condition-values cond))))
        (qss-sql--copy base (format "where %S" (qss-condition-form cond))
                       :wheres (append (qss-relation-wheres base) (list sql))))
    (cl-call-next-method)))

(cl-defmethod qss-sort-by ((rel qss-relation) &rest keys)
  "Sort the query REL by KEYS in SQL; KEYS as in the generic function.
Each :desc reverses the whole order so far, as in memory: the columns
after it and the earlier order run the other way, and on a table the
rowid breaks the last ties."
  (let* ((parsed (qss--sort-keys keys))
         (base (if (qss-relation-limit rel) (qss-sql--wrap rel) rel))
         (flip nil)
         ;; the columns are looked up now: a later select may drop them,
         ;; and ORDER BY may still name them
         (new (mapcar (lambda (k)
                        (setq flip (not (eq flip (cdr k))))
                        (list (car k) flip
                              (qss-sql--binary base (qss-sql--column base (car k))
                                               (nth 2 (assq (car k) (qss-relation-columns base))))))
                      parsed))
         (old (cl-remove-if (lambda (o) (assq (car o) parsed)) (qss-relation-order base)))
         (id (and (not (assq 'qss--rowid old)) (qss-sql--direct-p base)
                  (qss-sql-db-order-id (qss-relation-file base) (qss-relation-table base))))
         (old (if id
                  (append old (list (list 'qss--rowid nil id)))
                old)))
    (qss-sql--copy base (qss--sort-stage parsed)
                   :order (cl-remove-if
                           ;; rowid ascending is the order anyway
                           (lambda (o) (and (eq (car o) 'qss--rowid) (not (nth 1 o))))
                           (append new
                                   (mapcar (lambda (o) (list (car o) (if flip (not (nth 1 o)) (nth 1 o))
                                                             (nth 2 o)))
                                           old))))))

(cl-defmethod qss-first ((rel qss-relation) &optional n)
  "Limit the query REL to N rows in SQL; without N fetch the first row itself."
  (if (null n)
      (qss--row-itself (car (qss-sql--run (qss-first rel 1))))
    (qss-sql--copy rel (format "first %d" n)
                   :limit (if (qss-relation-limit rel) (min n (qss-relation-limit rel)) n))))

(cl-defmethod qss-count ((rel qss-relation))
  "Count the rows of the query REL in SQL."
  (let ((q (qss-sql-query rel)))
    (caar (qss-sql-db-select (qss-relation-file rel)
                             (format "SELECT COUNT(*) FROM (\n%s\n) AS t" (car q))
                             (cdr q)))))

(defun qss-sql--project (rel keys stage)
  "Return REL cut down to the columns KEYS (in this order), titled STAGE."
  (dolist (k keys) (qss-sql--column rel k))   ; do they exist?
  (qss-sql--copy rel stage
                 :columns (mapcar (lambda (k) (assq k (qss-relation-columns rel))) keys)
                 :projected t
                 ;; the rows are plain data again, no groups
                 :row-fn nil))

(cl-defmethod qss-select ((rel qss-relation) &rest cols)
  "Select the columns COLS in the SQL of REL."
  (let ((keys (mapcar #'qss--name cols)))
    (qss-sql--project rel keys (format "select %s" (mapconcat #'symbol-name keys " ")))))

(cl-defmethod qss-reject ((rel qss-relation) &rest cols)
  "Select all columns of REL except COLS, in SQL."
  (let ((keys (mapcar #'qss--name cols)))
    (dolist (k keys) (qss-sql--column rel k))   ; do they exist?
    (qss-sql--project rel
                      (cl-remove-if (lambda (k) (memq k keys))
                                    (mapcar #'car (qss-relation-columns rel)))
                      (format "reject %s" (mapconcat #'symbol-name keys " ")))))

(cl-defmethod qss-columns-of ((rel qss-relation))
  "Return the column names of REL from the schema, without fetching rows.
The same as in memory: one row per column, a single value."
  (qss-make-table (format "%s | columns" (qss-relation-title rel))
                  (mapcar (lambda (c) (qss--value 'column (car c)))
                          (qss-relation-columns rel))
                  '(column)))

;;; ------------------------------------------------------------------
;;; Join and group-sum as SQL

(defun qss-sql--from-query (rel stage columns select tail &optional extra-params)
  "Return a query over REL: SELECT SELECT FROM (REL) AS t TAIL.
COLUMNS are the columns of the new query, named by their keys; STAGE
extends the title.  EXTRA-PARAMS come after those of REL."
  (let ((q (qss-sql-query rel)))
    (qss-relation--make
     :file (qss-relation-file rel) :table (qss-relation-table rel)
     :columns columns :projected t
     :title (format "%s | %s" (qss-relation-title rel) stage)
     :source (cons (format "SELECT %s\nFROM (\n%s\n) AS t%s" select
                           (replace-regexp-in-string "^" "  " (car q)) tail)
                   (append (cdr q) extra-params)))))

(cl-defmethod qss-join ((rel qss-relation) other &rest options)
  "Join the query REL with OTHER in SQL, if OTHER is a query of the same file.
Otherwise in memory."
  (if (not (and (qss-relation-p other)
                (qss-sql--same-db-p (qss-relation-file other) (qss-relation-file rel))))
      (cl-call-next-method)
    (pcase-let* ((`(,on ,kind ,suffix ,prefix) (qss--join-args options))
                 (left-names (mapcar #'car (qss-relation-columns rel)))
                 (right-names (mapcar #'car (qss-relation-columns other)))
                 (pairs (qss--join-on on left-names right-names))
                 (right-cols (qss--join-columns left-names right-names pairs suffix prefix))
                 (lq (qss-sql-query rel))
                 (rq (qss-sql-query other))
                 (lcol (lambda (k) (qss-sql-quote (nth 1 (assq k (qss-relation-columns rel))))))
                 (rcol (lambda (k) (qss-sql-quote (nth 1 (assq k (qss-relation-columns other)))))))
      (qss-relation--make
       :file (qss-relation-file rel) :table (qss-relation-table rel) :projected t
       :columns (append (mapcar (lambda (c) (list (car c) (symbol-name (car c)) (nth 2 c) (nth 3 c)))
                                (qss-relation-columns rel))
                        (mapcar (lambda (c)
                                  (let ((col (assq (car c) (qss-relation-columns other))))
                                    (list (cdr c) (symbol-name (cdr c)) (nth 2 col) (nth 3 col))))
                                right-cols))
       :title (format "%s | join %s%s" (qss-relation-title rel) (qss-relation-title other)
                      (if (eq kind 'left) " (left)" ""))
       :source (cons (format "SELECT %s\nFROM (\n%s\n) AS l\n%s (\n%s\n) AS r\nON %s"
                             (mapconcat #'identity
                                        (append
                                         (mapcar (lambda (k) (format "l.%s AS %s" (funcall lcol k)
                                                                     (qss-sql-quote (symbol-name k))))
                                                 left-names)
                                         (mapcar (lambda (c) (format "r.%s AS %s" (funcall rcol (car c))
                                                                     (qss-sql-quote (symbol-name (cdr c)))))
                                                 right-cols))
                                        ", ")
                             (replace-regexp-in-string "^" "  " (car lq))
                             (if (eq kind 'left) "LEFT JOIN" "JOIN")
                             (replace-regexp-in-string "^" "  " (car rq))
                             ;; NULL matches NULL, as in Nushell
                             (mapconcat (lambda (p) (format "l.%s %s r.%s" (funcall lcol (car p))
                                                            (qss-sql--dialect rel :is)
                                                            (funcall rcol (cdr p))))
                                        pairs " AND "))
                     (append (cdr lq) (cdr rq)))))))

(defun qss-sql--group (rel keys aggs stage)
  "Return REL grouped by KEYS with AGGS, as a query; STAGE is the title.
The rows are groups (`qss-group') whose rows a query of their own fetches."
  (dolist (k keys) (qss-sql--column rel k))
  (dolist (a aggs) (qss-sql--column rel (nth 1 a)))
  (let* ((col (lambda (k) (qss-sql--column rel k)))
         (type-of-col (lambda (k) (assq k (qss-relation-columns rel))))
         (avg-fmt (qss-sql--dialect rel :avg))
         (agg-sql (lambda (a)
                    (let ((arg (funcall col (nth 1 a))))
                      (format "%s(%s) AS %s" (upcase (symbol-name (car a)))
                              (if (and avg-fmt (eq (car a) 'avg)) (format avg-fmt arg) arg)
                              (qss-sql-quote (symbol-name (nth 2 a)))))))
         (agg-type (lambda (a)
                     (let* ((c (funcall type-of-col (nth 1 a)))
                            (qtype (nth 3 c))
                            (numeric (eq (qss-sql--affinity (nth 2 c)) 'numeric)))
                       (pcase (car a)
                         ('count 'number)
                         ((or 'sum 'avg) (if (memq qtype '(size duration)) qtype 'number))
                         (_ (or qtype (and numeric 'number)))))))
         (columns (append (mapcar (lambda (k) (let ((c (funcall type-of-col k)))
                                                (list k (symbol-name k) (nth 2 c) (nth 3 c))))
                                  keys)
                          (list (list 'count "count" "INTEGER" 'number))
                          (mapcar (lambda (a) (list (nth 2 a) (symbol-name (nth 2 a)) ""
                                                    (funcall agg-type a)))
                                  aggs)))
         (specs (mapcar (lambda (c) (if (nth 3 c) (list (car c) :type (nth 3 c)) (car c))) columns))
         (row-columns (qss-sql--specs rel nil))
         (base rel)
         (grouped (qss-sql--from-query
                   rel stage columns
                   (mapconcat #'identity
                              (append (mapcar (lambda (k) (format "%s AS %s" (funcall col k)
                                                                  (qss-sql-quote (symbol-name k))))
                                              keys)
                                      (list "COUNT(*) AS \"count\"")
                                      (mapcar agg-sql aggs))
                              ", ")
                   (if keys
                       (format "\nGROUP BY %s" (mapconcat col keys ", "))
                     ""))))
    (setf (qss-relation-row-fn grouped)
          (lambda (row)
            (qss--make-group
             :keys (mapcar (lambda (k) (cons k (plist-get row (intern (format ":%s" k))))) keys)
             :count (plist-get row :count)
             :aggs (mapcar (lambda (a) (cons (nth 2 a) (plist-get row (intern (format ":%s" (nth 2 a))))))
                           aggs)
             :source base
             :specs (mapcar #'qss--column-spec specs)
             :row-columns row-columns
             :title (if keys
                        (mapconcat (lambda (k) (format "%s = %s" k (plist-get row (intern (format ":%s" k)))))
                                   keys ", ")
                      "all rows"))))
    ;; sorted by the keys, missing values last, as in memory
    (setf (qss-relation-order grouped)
          (mapcar (lambda (k) (list k nil (qss-sql--binary rel (qss-sql-quote (symbol-name k))
                                                           (nth 2 (assq k (qss-relation-columns rel))))))
                  keys))
    grouped))

(cl-defmethod qss-group-sum ((rel qss-relation) &rest args)
  "Group the query REL by ARGS in SQL (GROUP BY)."
  (pcase-let ((`(,keys . ,aggs) (qss--group-args args)))
    (unless keys (user-error "group-sum needs a column; for all rows use summarize"))
    (qss-sql--group rel keys aggs (qss--group-stage "group-sum" args))))

(cl-defmethod qss-summarize ((rel qss-relation) &rest aggs)
  "Summarize the query REL by AGGS in SQL."
  (pcase-let ((`(,keys . ,aggs*) (qss--group-args aggs)))
    (when keys (user-error "summarize takes only aggregates such as (sum amount); for groups use group-sum"))
    (qss-sql--group rel nil aggs* (qss--group-stage "summarize" aggs))))

(defun qss-sql (rel)
  "Return the query REL as text with its parameters."
  (unless (qss-relation-p rel)
    (user-error "The pipe is no longer a query at this point"))
  (let ((q (qss-sql-query rel)))
    (format "%s\nParameters: %S" (car q) (cdr q))))

;;; ------------------------------------------------------------------
;;; Source: the tables of a database

(cl-defstruct (qss-db-table (:constructor qss-db-table--make) (:copier nil))
  "A table of the database FILE (an SQLite file or another): NAME, COLUMNS."
  file name columns)

(defun qss-sql-db-tables (db)
  "Return the tables of the database DB as a table of `qss-db-table' rows."
  (qss-make-table (format "tables %s" (qss-sql-db-label db))
                  (mapcar (lambda (tab) (qss-db-table--make :file db :name (car tab)
                                                            :columns (cdr tab)))
                          (qss-sql-db-schema db))
                  nil
                  (lambda () (qss-sql-db-tables db))))

(defun qss-tables (&optional file)
  "Return the tables of the SQLite file FILE (default `qss-database').
RET on one of them shows its rows."
  (let ((file (qss-sql--file (or file (qss-database-file)))))
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
  (qss-display (qss-sql-relation (qss-db-table-file row) (qss-db-table-name row))))

(cl-defmethod qss-actions ((row qss-db-table))
  "Return the actions for the database table ROW."
  (ignore row)
  (list (cons "Show rows" #'qss-default-action)
        (cons "Show columns"
              (lambda (r) (qss-show (qss-columns-of (qss-sql-relation (qss-db-table-file r)
                                                                      (qss-db-table-name r))))))
        (cons "Count" (lambda (r) (message "%s: %d rows" (qss-db-table-name r)
                                           (qss-count (qss-sql-relation (qss-db-table-file r)
                                                                        (qss-db-table-name r))))))))

;;; ------------------------------------------------------------------
;;; Writing: the word `into'

(defun qss-sql--store-value (v spec)
  "Return V of the column SPEC as a value SQLite can store.
Times as text in `qss-time-format', t as 1, symbols by name, other
Lisp values (lists, records) as their printed text."
  (cond ((null v) nil)
        ((or (stringp v) (integerp v) (floatp v))
         (if (and (numberp v) (eq (plist-get (cdr spec) :type) 'time))
             (format-time-string qss-time-format v)
           v))
        ((qss--time-p v) (format-time-string qss-time-format v))
        ((eq v t) 1)
        ((symbolp v) (symbol-name v))
        (t (qss-format-value v))))

(defun qss-sql--column-types (spec values)
  "Return (SQL-TYPE . QSS-TYPE) for the column SPEC with VALUES.
Sizes and durations are INTEGER (bytes, nanoseconds), times TEXT;
otherwise the values decide: INTEGER, REAL or TEXT."
  (let* ((type (plist-get (cdr spec) :type))
         (raw (delq nil (copy-sequence values)))
         (type (or type (and raw (cl-every #'qss--time-p raw) 'time)))
         ;; the values as stored: t becomes 1, a symbol text ...
         (present (mapcar (lambda (v) (qss-sql--store-value v spec)) raw)))
    (cons (cond ((memq type '(size duration)) "INTEGER")
                ((eq type 'time) "TEXT")
                ((and present (cl-every #'integerp present)) "INTEGER")
                ((and present (cl-every #'numberp present)) "REAL")
                (t "TEXT"))
          type)))

(defun qss-sql--stored-specs (table)
  "Return the column specs TABLE stores: its columns and `qss-stored-fields'."
  (let* ((specs (qss-table-columns table))
         (row (car (qss-table-rows table)))
         (extra (and row (cl-remove-if (lambda (f) (assq f specs)) (qss-stored-fields row)))))
    (append specs (mapcar #'qss--column-spec extra))))

(defun qss-sql-write (table file name &optional mode)
  "Write the rows of TABLE into the table NAME of the SQLite FILE.
FILE is created if needed.  If NAME exists, MODE decides: `replace'
drops it first, `append' adds the rows; nil signals an error.  All in
one transaction.  The qss types of the columns are recorded, so the
table shows sizes and durations as such again.  Return the number of
rows written."
  (qss-sql--check)
  (let* ((file (expand-file-name file))
         (specs (qss-sql--stored-specs table))
         (rows (qss-table-rows table))
         (quoted (qss-sql-quote name)))
    (unless specs (user-error "Nothing to store: the table has no columns"))
    (make-directory (file-name-directory file) t)
    (qss-sql--with-db db file
      (with-sqlite-transaction db
        (let ((exists (sqlite-select
                       db "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ? COLLATE NOCASE"
                       (vector name))))
          (when (and exists (null mode))
            (user-error "Table %s exists in %s: add :replace or :append"
                        name (abbreviate-file-name file)))
          (when (and exists (eq mode 'replace))
            (sqlite-execute db (format "DROP TABLE %s" quoted)))
          (unless (and exists (eq mode 'append))
            (let ((types (mapcar (lambda (spec)
                                   (qss-sql--column-types
                                    spec (mapcar (lambda (r) (qss-field r (car spec))) rows)))
                                 specs)))
              (sqlite-execute db (format "CREATE TABLE %s (%s)" quoted
                                         (mapconcat (lambda (st)
                                                      (format "%s %s" (qss-sql-quote (symbol-name (car (car st))))
                                                              (cdr st)))
                                                    (cl-mapcar #'cons specs (mapcar #'car types))
                                                    ", ")))
              (sqlite-execute db (format "CREATE TABLE IF NOT EXISTS %s
                                            (\"table\" TEXT, \"column\" TEXT, \"type\" TEXT,
                                             PRIMARY KEY (\"table\", \"column\"))"
                                         (qss-sql-quote qss-sql--types-table)))
              (sqlite-execute db (format "DELETE FROM %s WHERE \"table\" = ? COLLATE NOCASE"
                                         (qss-sql-quote qss-sql--types-table))
                              (vector name))
              (cl-mapc (lambda (spec type)
                         (when (cdr type)
                           (sqlite-execute db (format "INSERT INTO %s VALUES (?, ?, ?)"
                                                      (qss-sql-quote qss-sql--types-table))
                                           (vector name (symbol-name (car spec))
                                                   (symbol-name (cdr type))))))
                       specs types)))
          (let ((insert (format "INSERT INTO %s (%s) VALUES (%s)" quoted
                                (mapconcat (lambda (s) (qss-sql-quote (symbol-name (car s)))) specs ", ")
                                (mapconcat (lambda (_) "?") specs ", "))))
            (dolist (row rows)
              (sqlite-execute db insert
                              (vconcat (mapcar (lambda (spec)
                                                 (qss-sql--store-value (qss-field row (car spec)) spec))
                                               specs))))))))
    (remhash file qss-sql--schemas)
    (length rows)))

(defun qss-sql--into-args (args)
  "Return (NAME FILE MODE) from the ARGS of (into sqlite NAME ...).
Keywords: :file FILE, :replace, :append (with or without a value)."
  (let ((name (pop args)) (file (qss-database-file)) (mode nil))
    (unless (or (stringp name) (and name (symbolp name)))
      (user-error "into sqlite: first the name of the table, e.g. (into sqlite \"files\")"))
    (while args
      (let ((key (pop args))
            (value (if (and args (not (keywordp (car args)))) (pop args) t)))
        (pcase key
          (:file (setq file value))
          (:replace (when value (setq mode 'replace)))
          (:append (when value (setq mode 'append)))
          (_ (user-error "into sqlite: unknown option %S (known: :file :replace :append)" key)))))
    (list (if (symbolp name) (symbol-name name) name) file mode)))

(cl-defmethod qss-into (table (_target (eql sqlite)) &rest args)
  "Store TABLE in SQLite and return the stored table as a query.
ARGS: NAME, then :file FILE (default `qss-database'), :replace or
:append.  Without :replace or :append, NAME must not exist yet."
  (pcase-let ((`(,name ,file ,mode) (qss-sql--into-args args)))
    (qss-sql-write (qss-as-table table) file name mode)
    (qss-sqlite file name)))

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
