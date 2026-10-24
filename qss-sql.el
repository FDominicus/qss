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

(defcustom qss-database (locate-user-emacs-file "qss.sqlite3")
  "The central SQLite file of qss.
\=(into sqlite \"Name\") stores there, (sqlite \"Name\") and (tables)
read from there, unless a file is given."
  :type 'file
  :group 'qss)

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
select/reject; then the query names its columns.  TITLE: the pipe so far.
SOURCE: (SQL . PARAMS) of a query this one reads from instead of TABLE
\(after join and group-by); its columns are named by their keys.
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
          file qss-database))
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

(defun qss-sql--param (value params)
  "Append VALUE to PARAMS and return \"?\"."
  (nconc params (list (qss-sql--value value)))
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
        (quantities nil))                 ; inside a comparison that converts
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

(defun qss-sql-query (rel &optional select)
  "Return (SQL . PARAMS) of the query REL.
SELECT, if given, replaces the list of columns (e.g. \"rowid\")."
  (let* ((inner (cond ((qss-relation-inner rel) (qss-sql-query (qss-relation-inner rel)))
                      ((qss-relation-source rel))))
         (from (if inner
                   (format "(\n%s\n) AS t" (replace-regexp-in-string "^" "  " (car inner)))
                 (qss-sql-quote (qss-relation-table rel))))
         (wheres (qss-relation-wheres rel))
         (order (qss-relation-order rel))
         (limit (qss-relation-limit rel)))
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

(defvar qss-sql--rowids (make-hash-table :test #'eq :weakness 'key)
  "Row -> its rowid, for rows fetched straight from a table.
Editing (qss-edit.el) and `qss-sql-rowid' use it.")

(defun qss-sql-rowid (row)
  "Return the rowid of ROW, fetched from an SQLite table, or nil."
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
                       (qss-relation-columns rel))))
    (qss-sql--with-db db (qss-relation-file rel)
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
        (if (qss-relation-row-fn rel)
            (mapcar (qss-relation-row-fn rel) rows)
          rows)))))

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

;;; ------------------------------------------------------------------
;;; Join and group-by as SQL

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
                (equal (qss-relation-file other) (qss-relation-file rel))))
      (cl-call-next-method)
    (pcase-let* ((`(,on . ,kind) (qss--join-args options))
                 (left-names (mapcar #'car (qss-relation-columns rel)))
                 (right-names (mapcar #'car (qss-relation-columns other)))
                 (pairs (qss--join-on on left-names right-names))
                 (right-cols (qss--join-columns left-names right-names pairs))
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
                             (mapconcat (lambda (p) (format "l.%s = r.%s" (funcall lcol (car p))
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
         (agg-sql (lambda (a)
                    (format "%s(%s) AS %s" (upcase (symbol-name (car a))) (funcall col (nth 1 a))
                            (qss-sql-quote (symbol-name (nth 2 a))))))
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
          (mapcar (lambda (k) (list k nil (qss-sql-quote (symbol-name k)))) keys))
    grouped))

(cl-defmethod qss-group-by ((rel qss-relation) &rest args)
  "Group the query REL by ARGS in SQL (GROUP BY)."
  (pcase-let ((`(,keys . ,aggs) (qss--group-args args)))
    (unless keys (user-error "group-by needs a column; for all rows use summarize"))
    (qss-sql--group rel keys aggs (qss--group-stage "group-by" args))))

(cl-defmethod qss-summarize ((rel qss-relation) &rest aggs)
  "Summarize the query REL by AGGS in SQL."
  (pcase-let ((`(,keys . ,aggs*) (qss--group-args aggs)))
    (when keys (user-error "summarize takes only aggregates such as (sum amount); for groups use group-by"))
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
  "A table of the SQLite file FILE: its NAME and COLUMNS."
  file name columns)

(defun qss-tables (&optional file)
  "Return the tables of the SQLite file FILE (default `qss-database').
RET on one of them shows its rows."
  (let ((file (qss-sql--file (or file qss-database))))
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
  (let ((name (pop args)) (file qss-database) (mode nil))
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
