;;; qss-pipe.el --- One vocabulary for all tables, after Nushell  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; Layer 2 of qss: words that work the same on every `qss-table',
;; whether the rows are files, processes or property lists.
;;
;;   (qss-> (files "~")
;;          (where (> :size 10000))
;;          (sort-by modified :desc)
;;          (select name size modified)
;;          (first 10))
;;
;; Words:  where  select  reject  sort-by  first  last  get  each
;;         group-by  columns  count  collect (the rows as a list)
;;         to (the table as text: (to org), (to csv) ...)
;;
;; Conditions (`where'):
;;   - Without `it' they are data: keywords are columns, everything else
;;     is Lisp (variables too).  Comparisons mean what they mean in SQL,
;;     so the same condition can become SQL (see qss-sql.el):
;;       =  /=  <  >  <=  >=   on numbers, strings and times
;;                             (a time against "2026-10-20" works)
;;       (like :name "%.el")   as in SQL, ignoring case
;;       (in :type '("dir" "link"))
;;       (is-null :x)  (not-null :x)
;;       and  or  not
;;     A missing value (nil) is NULL: comparing with it is unknown, and
;;     `where' keeps only rows where the condition is true.
;;   - If `it' occurs, the condition is ordinary Lisp and `it' is the
;;     row object:  (where (string-prefix-p "." (qss-field it 'name)))
;;
;; Column names in select, sort-by, get ... are symbols: (sort-by size :desc).
;;
;; Every word returns a table again (except `count', `collect' and `to').
;; Rows stay their objects: a filtered file still opens with RET.  A
;; derived table knows the way back to its source; \\`g' in its buffer
;; reads the source again and applies the words once more.
;;
;; M-x qss-eval reads forms without `qss->' and shows the result:
;;   qss: (files "~") (where (> :size 100000)) (sort-by size :desc)

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'qss-table)

;;; ------------------------------------------------------------------
;;; Deriving: a new table from an old one, with the way back to the source

(cl-defgeneric qss-as-table (value)
  "Return VALUE as a table with its rows in memory.
A table stays a table, a list becomes the rows.  Other sources (such
as a query in qss-sql.el) fetch their rows here."
  (error "Not a table: %S" value))

(cl-defmethod qss-as-table ((value qss-table))
  "Return VALUE, which is a table already."
  value)
(cl-defmethod qss-as-table ((value list))
  "Return a table whose rows are the elements of VALUE."
  (qss-make-table "list" value))

(cl-defgeneric qss-as-input (value)
  "Return VALUE as input of the first stage of `qss->'.
The default is `qss-as-table'; a query stays a query, so the following
words can still become SQL."
  (qss-as-table value))

(cl-defgeneric qss-display (value)
  "Show VALUE: tables in a buffer, everything else as a message."
  (message "%s" (if (stringp value) value (format "%S" value))))

(cl-defmethod qss-display ((value qss-table))
  "Show the table VALUE in a buffer."
  (qss-show value))

(defun qss--derive (table stage rows &optional columns again)
  "Derive a new table from TABLE: title extended by STAGE, rows ROWS.
COLUMNS are the columns (otherwise those of TABLE, or of the first row).
AGAIN is a function that applies the same word to TABLE read anew."
  (let ((parent-source (qss-table-source table)))
    (qss-make-table (format "%s | %s" (qss-table-title table) stage)
                    rows
                    (or columns
                        (and rows (not (qss--same-kind-p table rows))
                             (qss-columns (car rows)))
                        (qss-table-columns table))
                    (and parent-source again
                         (lambda () (funcall again (funcall parent-source)))))))

(defun qss--same-kind-p (table rows)
  "Return non-nil if ROWS are of the same kind as the rows of TABLE."
  (let ((old (car (qss-table-rows table))))
    (or (null old) (eq (type-of old) (type-of (car rows))))))

(defun qss--name (col)
  "Return the column name COL as a symbol, from a symbol, keyword or string."
  (cond ((keywordp col) (intern (substring (symbol-name col) 1)))
        ((symbolp col) col)
        ((stringp col) (intern col))
        (t (error "Not a column name: %S" col))))

(defun qss--spec-of (table name)
  "Return the column spec NAME of TABLE, or a plain one."
  (or (assq name (qss-table-columns table))
      (qss--column-spec name)))

;;; ------------------------------------------------------------------
;;; Comparisons with the meaning of SQL

(defun qss--time-p (v)
  "Return non-nil if V is a Lisp time value, as from `file-attributes'."
  (and (consp v) (integerp (car v))
       (or (integerp (cdr v)) (and (consp (cdr v)) (integerp (cadr v))))))

(defun qss--to-time (s)
  "Return text S like \"2026-10-20\" or \"2026-10-20 07:30\" as a Lisp time.
The time is local time."
  (condition-case nil
      (encode-time (decoded-time-set-defaults
                    (iso8601-parse (string-replace " " "T" s))))
    (error (condition-case nil
               (encode-time (decoded-time-set-defaults (iso8601-parse-date s)))
             (error (user-error "Not a date: %S" s))))))

(defun qss--compare (a b)
  "Return -1, 0 or 1 comparing A with B; nil if A or B is missing (NULL)."
  (cond ((or (null a) (null b)) nil)
        ((and (numberp a) (numberp b)) (cond ((< a b) -1) ((> a b) 1) (t 0)))
        ;; sizes and durations as Nushell writes them: (> :size "10kb")
        ((and (numberp a) (qss-parse-quantity b)) (qss--compare a (cdr (qss-parse-quantity b))))
        ((and (numberp b) (qss-parse-quantity a)) (qss--compare (cdr (qss-parse-quantity a)) b))
        ((and (stringp a) (stringp b)) (cond ((string< a b) -1) ((string= a b) 0) (t 1)))
        ((or (qss--time-p a) (qss--time-p b))
         (let ((a (if (stringp a) (qss--to-time a) a))
               (b (if (stringp b) (qss--to-time b) b)))
           (cond ((time-less-p a b) -1) ((time-equal-p a b) 0) (t 1))))
        ((and (symbolp a) (symbolp b)) (qss--compare (symbol-name a) (symbol-name b)))
        (t (user-error "Cannot compare %S with %S" a b))))

(defun qss-op-= (a b)
  "Return non-nil if A equals B (numbers, strings, times); NULL is never equal."
  (let ((c (if (or (stringp a) (stringp b) (numberp a) (numberp b) (qss--time-p a))
               (ignore-errors (qss--compare a b))
             (and a b (if (equal a b) 0 1)))))
    (eql c 0)))

(defun qss-op-/= (a b)
  "Return non-nil if A and B are both present and differ."
  (and a b (not (qss-op-= a b))))

(defun qss-op-< (a b)
  "Return non-nil if A is less than B."
  (eql -1 (qss--compare a b)))

(defun qss-op-> (a b)
  "Return non-nil if A is greater than B."
  (eql 1 (qss--compare a b)))

(defun qss-op-<= (a b)
  "Return non-nil if A is less than or equal to B."
  (and (memql (qss--compare a b) '(-1 0)) t))

(defun qss-op->= (a b)
  "Return non-nil if A is greater than or equal to B."
  (and (memql (qss--compare a b) '(0 1)) t))

(defun qss-op-is-null (a)
  "Return non-nil if A is missing."
  (null a))

(defun qss-op-not-null (a)
  "Return non-nil if A is present."
  (not (null a)))

(defun qss-op-in (a list)
  "Return non-nil if A equals an element of LIST (by `qss-op-=')."
  (and a (cl-some (lambda (x) (qss-op-= a x)) list) t))

(defun qss--like-regexp (pattern)
  "Return the SQL LIKE PATTERN as a regexp."
  (concat "\\`"
          (mapconcat (lambda (c)
                       (pcase c
                         (?% ".*")
                         (?_ ".")
                         (_ (regexp-quote (char-to-string c)))))
                     pattern "")
          "\\'"))

(defun qss-op-like (a pattern)
  "Return non-nil if A matches the SQL LIKE PATTERN.
% is any number of characters, _ exactly one; case is ignored."
  (and (stringp a)
       (let ((case-fold-search t))
         (string-match-p (qss--like-regexp pattern) a))
       t))

(defconst qss--operators
  '((= . qss-op-=) (/= . qss-op-/=) (< . qss-op-<) (> . qss-op->) (<= . qss-op-<=) (>= . qss-op->=)
    (like . qss-op-like) (in . qss-op-in) (is-null . qss-op-is-null) (not-null . qss-op-not-null))
  "Operators in conditions as data and their functions.")

;;; ------------------------------------------------------------------
;;; Conditions

(cl-defstruct (qss-condition (:constructor qss--make-condition
                                           (form fn &optional template values))
                             (:copier nil))
  "A condition.
FORM: as it was written (for the title).
FN: the function of a row (for rows in memory).
TEMPLATE: FORM with (qss-param N) in place of every value, or nil if the
condition cannot become SQL (Lisp with `it', functions over columns).
VALUES: the values, evaluated at run time, in the order of N."
  form fn template values)

(defun qss--mentions-it-p (form)
  "Return non-nil if the symbol `it' occurs in FORM outside of quote."
  (cond ((eq form 'it) t)
        ((and (consp form) (memq (car form) '(quote function))) nil)
        ((consp form) (or (qss--mentions-it-p (car form))
                          (qss--mentions-it-p (cdr form))))
        (t nil)))

;; Three-valued logic as in SQL: a comparison with NULL is neither true
;; nor false but unknown (`qss-unknown'); `not' leaves unknown unknown,
;; and `where' keeps only what is true.  So (not (= :city "Bruchsal"))
;; returns the same rows in memory as in SQL: not those without a city.

(defconst qss--null-ops '(= /= < > <= >= like in)
  "Operators that return unknown when given NULL.")

(defun qss--null-safe (fn a b)
  "Call FN on A and B; unknown if A is missing (for `in' only A counts) or B is."
  (if (or (null a) (and (null b) (not (eq fn #'qss-op-in))))
      'qss-unknown
    (and (funcall fn a b) t)))

(defun qss--true-p (v)
  "Return non-nil if V is true, neither false nor unknown."
  (and v (not (eq v 'qss-unknown))))

(defun qss--and3 (&rest vs)
  "Three-valued AND of VS."
  (cond ((memq nil vs) nil)
        ((memq 'qss-unknown vs) 'qss-unknown)
        (t t)))

(defun qss--or3 (&rest vs)
  "Three-valued OR of VS."
  (cond ((cl-some #'qss--true-p vs) t)
        ((memq 'qss-unknown vs) 'qss-unknown)
        (t nil)))

(defun qss--not3 (v)
  "Three-valued NOT of V."
  (cond ((eq v 'qss-unknown) v) (v nil) (t t)))

(defun qss--translate (form)
  "Translate the condition FORM (as data) into Lisp over `it', three-valued."
  (cond ((and (keywordp form) form)
         `(qss-field it ',(qss--name form)))
        ((atom form) form)
        ((memq (car form) '(quote function)) form)
        ((memq (car form) qss--null-ops)
         `(qss--null-safe #',(alist-get (car form) qss--operators)
                          ,@(mapcar #'qss--translate (cdr form))))
        ((eq (car form) 'and) `(qss--and3 ,@(mapcar #'qss--translate (cdr form))))
        ((eq (car form) 'or) `(qss--or3 ,@(mapcar #'qss--translate (cdr form))))
        ((eq (car form) 'not) `(qss--not3 ,(qss--translate (cadr form))))
        (t (let ((op (alist-get (car form) qss--operators)))
             (cons (or op (car form)) (mapcar #'qss--translate (cdr form)))))))

(defun qss--mentions-column-p (form)
  "Return non-nil if a keyword (a column) occurs in FORM outside of quote."
  (cond ((keywordp form) t)
        ((and (consp form) (memq (car form) '(quote function))) nil)
        ((consp form) (or (qss--mentions-column-p (car form))
                          (qss--mentions-column-p (cdr form))))
        (t nil)))

(defun qss--sql-template (form)
  "Return (TEMPLATE . VALUE-FORMS) for FORM, or nil if FORM cannot become SQL.
Columns, operators and and/or/not stay in the template; every other
subform without a column is a value and becomes (qss-param N).  A
function over columns, e.g. (string-prefix-p \"a\" :name), cannot
become SQL."
  (let ((values nil) (ok t))
    (cl-labels ((walk (f)
                  (cond ((keywordp f) f)
                        ((and (consp f) (or (memq (car f) '(and or not))
                                            (assq (car f) qss--operators)))
                         (cons (car f) (mapcar #'walk (cdr f))))
                        ((qss--mentions-column-p f) (setq ok nil) f)
                        (t (push f values)
                           (list 'qss-param (1- (length values)))))))
      (let ((template (walk form)))
        (and ok (cons template (nreverse values)))))))

(defun qss--condition-code (form)
  "Return code that yields FORM as a `qss-condition' at run time."
  (let ((sql (and (not (qss--mentions-it-p form)) (qss--sql-template form))))
    `(qss--make-condition ',form
                          (lambda (it)
                            (ignore it)
                            ,(if (qss--mentions-it-p form)
                                 form
                               `(qss--true-p ,(qss--translate form))))
                          ',(car sql)
                          ,(and sql `(list ,@(cdr sql))))))

(defun qss--predicate (cond)
  "Return the row predicate of COND (a `qss-condition' or a function)."
  (cond ((qss-condition-p cond) (qss-condition-fn cond))
        ((functionp cond) cond)
        (t (error "Not a condition: %S" cond))))

(defun qss--describe-condition (cond)
  "Return COND as text for a title."
  (if (qss-condition-p cond) (format "%S" (qss-condition-form cond)) "<function>"))

;;; ------------------------------------------------------------------
;;; Single values and groups as rows

(cl-defstruct (qss-value (:constructor qss--value (column value)) (:copier nil))
  "A single VALUE as a row with one column COLUMN."
  column value)

(cl-defmethod qss-columns ((row qss-value))
  "Return the one column of ROW."
  (list (qss-value-column row)))
(cl-defmethod qss-field ((row qss-value) _name)
  "Return the value of ROW, whatever field is asked for."
  (qss-value-value row))
(cl-defmethod qss-default-action ((row qss-value))
  "Copy the value of ROW to the kill ring."
  (let ((v (qss-value-value row)))
    (kill-new (format "%s" v))
    (message "Copied: %s" v)))

(cl-defstruct (qss-group (:constructor qss--make-group) (:copier nil))
  "A group from `qss-group-by': the rows whose COLUMN has the value KEY."
  column key rows columns title)

(cl-defmethod qss-columns ((row qss-group))
  "Return the columns of the group ROW: its key column and the count."
  (list (qss-group-column row) '(count :type number)))

(cl-defmethod qss-field ((row qss-group) name)
  "Return field NAME of the group ROW."
  (cond ((eq name (qss-group-column row)) (qss-group-key row))
        ((eq name 'count) (length (qss-group-rows row)))
        ((eq name 'rows) (qss-group-rows row))))

(cl-defmethod qss-default-action ((row qss-group))
  "Show the rows of the group ROW."
  (qss-show (qss-make-table (qss-group-title row) (qss-group-rows row)
                            (qss-group-columns row))))

;;; ------------------------------------------------------------------
;;; Words

(cl-defgeneric qss-where (table cond)
  "Keep the rows of TABLE that satisfy COND.
COND is a `qss-condition' (from `qss->') or a function of a row.")

(cl-defmethod qss-where (table cond)
  "Filter the rows of TABLE in memory by COND."
  (let ((table (qss-as-table table))
        (pred (qss--predicate cond)))
    (qss--derive table (format "where %s" (qss--describe-condition cond))
                 (cl-remove-if-not pred (qss-table-rows table))
                 (qss-table-columns table)
                 (lambda (tab) (qss-where tab cond)))))

(defun qss--data-rows-p (table)
  "Return non-nil if the rows of TABLE are data (property lists), not objects."
  (let ((row (car (qss-table-rows table))))
    (and (consp row) (keywordp (car row)))))

(defun qss--project (table names stage again)
  "Return TABLE restricted to the columns NAMES.
Data rows are cut down, objects stay whole.  STAGE and AGAIN are as
in `qss--derive'."
  (if (qss--data-rows-p table)
      (let ((known (mapcar #'car (qss-table-columns table))))
        (dolist (n names)
          (unless (memq n known)
            (user-error "No column %s; columns: %s" n
                        (mapconcat #'symbol-name known " "))))
        (qss--derive table stage
                     (mapcar (lambda (row)
                               (cl-mapcan (lambda (n) (list (intern (format ":%s" n))
                                                            (qss-field row n)))
                                          names))
                             (qss-table-rows table))
                     (mapcar (lambda (n) (qss--spec-of table n)) names)
                     again))
    (qss--derive table stage (qss-table-rows table)
                 (mapcar (lambda (n) (qss--spec-of table n)) names)
                 again)))

(cl-defgeneric qss-select (table &rest cols)
  "Return TABLE with only the columns COLS.
Data (property lists, rows from SQL) is cut down to these columns, as
in SQL and Nushell.  Objects (files, processes) stay whole so that RET
and \\`a' keep working; only the display shows the selection, and
`qss-field' still reaches all fields.")

(cl-defmethod qss-select (table &rest cols)
  "Keep the columns COLS of TABLE, in memory."
  (let* ((table (qss-as-table table))
         (names (mapcar #'qss--name cols)))
    (qss--project table names
                  (format "select %s" (mapconcat #'symbol-name names " "))
                  (lambda (tab) (apply #'qss-select tab cols)))))

(cl-defgeneric qss-reject (table &rest cols)
  "Return TABLE with all columns except COLS (cut down as in `qss-select').")

(cl-defmethod qss-reject (table &rest cols)
  "Drop the columns COLS of TABLE, in memory."
  (let* ((table (qss-as-table table))
         (names (mapcar #'qss--name cols)))
    (qss--project table
                  (cl-remove-if (lambda (n) (memq n names))
                                (mapcar #'car (qss-table-columns table)))
                  (format "reject %s" (mapconcat #'symbol-name names " "))
                  (lambda (tab) (apply #'qss-reject tab cols)))))

(defun qss--descending-p (dir)
  "Return non-nil if DIR asks for descending order."
  (pcase dir
    ((or 'nil :asc 'asc) nil)
    ((or :desc 'desc '-r :r) t)
    (_ (user-error "Direction %S: use :asc or :desc" dir))))

(cl-defgeneric qss-sort-by (table col &optional dir)
  "Return TABLE sorted by column COL; DIR is :asc (default) or :desc.
Sorting uses the raw value; missing values always come last.")

(cl-defmethod qss-sort-by (table col &optional dir)
  "Sort TABLE by COL in memory; DIR as in the generic function."
  (let* ((table (qss-as-table table))
         (name (qss--name col))
         (desc (qss--descending-p dir))
         (rows (sort (copy-sequence (qss-table-rows table))
                     (lambda (r1 r2)
                       (let ((a (qss-field r1 name)) (b (qss-field r2 name)))
                         (cond ((null a) nil)
                               ((null b) t)
                               (desc (eql 1 (qss--compare a b)))
                               (t (eql -1 (qss--compare a b)))))))))
    (qss--derive table (format "sort-by %s%s" name (if desc " desc" ""))
                 rows (qss-table-columns table)
                 (lambda (tab) (qss-sort-by tab col dir)))))

(cl-defgeneric qss-first (table &optional n)
  "Return the first N rows of TABLE (default 1).")

(cl-defmethod qss-first (table &optional n)
  "Take the first N rows of TABLE in memory."
  (let ((table (qss-as-table table)) (n (or n 1)))
    (qss--derive table (format "first %d" n)
                 (take n (qss-table-rows table)) (qss-table-columns table)
                 (lambda (tab) (qss-first tab n)))))

(cl-defgeneric qss-last (table &optional n)
  "Return the last N rows of TABLE (default 1).")

(cl-defmethod qss-last (table &optional n)
  "Take the last N rows of TABLE in memory."
  (let ((table (qss-as-table table)) (n (or n 1)))
    (qss--derive table (format "last %d" n)
                 (last (qss-table-rows table) n) (qss-table-columns table)
                 (lambda (tab) (qss-last tab n)))))

(cl-defgeneric qss-count (table)
  "Return the number of rows of TABLE.")

(cl-defmethod qss-count (table)
  "Count the rows of TABLE in memory."
  (length (qss-table-rows (qss-as-table table))))

(cl-defgeneric qss-collect (table)
  "Return the rows of TABLE as a Lisp list, at the end of a pipe.
Data rows are property lists; files and processes are the objects
themselves; after `get' they are the plain values.  A query (qss-sql)
runs here.")

(cl-defmethod qss-collect (table)
  "Return the rows of TABLE as a list; single values unwrapped."
  (mapcar (lambda (row) (if (qss-value-p row) (qss-value-value row) row))
          (qss-table-rows (qss-as-table table))))

(cl-defgeneric qss-get (table col)
  "Return the values of column COL of TABLE as a table with one column.")

(cl-defmethod qss-get (table col)
  "Return the values of COL of TABLE, in memory."
  (let* ((table (qss-as-table table))
         (name (qss--name col)))
    (qss--derive table (format "get %s" name)
                 (mapcar (lambda (r) (qss--value name (qss-field r name)))
                         (qss-table-rows table))
                 (list (qss--spec-of table name))
                 (lambda (tab) (qss-get tab col)))))

(cl-defgeneric qss-columns-of (table)
  "Return the column names of TABLE as a table.")

(cl-defmethod qss-columns-of (table)
  "Return the column names of TABLE."
  (let ((table (qss-as-table table)))
    (qss--derive table "columns"
                 (mapcar (lambda (spec) (qss--value 'column (car spec)))
                         (qss-table-columns table))
                 '(column)
                 #'qss-columns-of)))

(cl-defgeneric qss-each (table fn)
  "Call FN on every row of TABLE; the results are the new rows.")

(cl-defmethod qss-each (table fn)
  "Call FN on each row of TABLE, in memory."
  (let* ((table (qss-as-table table))
         (rows (mapcar (lambda (r)
                         (let ((v (funcall fn r)))
                           (if (or (consp v) (recordp v))
                               v
                             (qss--value 'value v))))
                       (qss-table-rows table))))
    ;; New rows, so new columns too (from the first row)
    (qss--derive table "each" rows
                 (if rows (qss-columns (car rows)) '(value))
                 (lambda (tab) (qss-each tab fn)))))

(cl-defgeneric qss-group-by (table col)
  "Group the rows of TABLE by the value of COL.
One row per value with the count; RET on a group shows its rows.")

(cl-defmethod qss-group-by (table col)
  "Group the rows of TABLE by COL, in memory."
  (let* ((table (qss-as-table table))
         (name (qss--name col))
         (groups nil))
    (dolist (r (qss-table-rows table))
      (let* ((k (qss-field r name))
             (cell (assoc k groups)))
        (if cell (push r (cdr cell)) (push (list k r) groups))))
    (qss--derive table (format "group-by %s" name)
                 (mapcar (lambda (g)
                           (qss--make-group :column name :key (car g)
                                            :rows (nreverse (cdr g))
                                            :columns (qss-table-columns table)
                                            :title (format "%s = %s" name (car g))))
                         (nreverse groups))
                 (list (qss--spec-of table name) '(count :type number))
                 (lambda (tab) (qss-group-by tab col)))))

;;; ------------------------------------------------------------------
;;; Tables as text: the word `to'

(defconst qss-text-formats '(text org md csv tsv)
  "Formats of the word `to' (`qss-to').")

(defun qss--plain-value (value spec)
  "Return VALUE of the column SPEC as plain text, for CSV and TSV.
Numbers stay exact (sizes in bytes, durations in nanoseconds), times
are written as 2026-10-22 07:11:05, a missing value is empty."
  (cond ((null value) "")
        ((stringp value) value)
        ((or (qss--time-p value)
             (and (numberp value) (eq (plist-get (cdr spec) :type) 'time)))
         (format-time-string "%Y-%m-%d %H:%M:%S" value))
        ((numberp value) (number-to-string value))
        (t (qss-format-value value))))

(defun qss--one-line (text)
  "Return TEXT with line breaks and tabs as spaces."
  (replace-regexp-in-string "[\n\r\t]" " " text))

(defun qss--text-cells (table plain)
  "Return the header and the rows of TABLE as lists of strings.
With PLAIN, the cells are plain values (`qss--plain-value'), otherwise
the text the table shows."
  (let ((specs (qss-table-columns table)))
    (cons (mapcar (lambda (spec) (symbol-name (car spec))) specs)
          (mapcar (lambda (row)
                    (mapcar (lambda (spec)
                              (if plain
                                  (qss--plain-value (qss-field row (car spec)) spec)
                                (qss--display-value row spec)))
                            specs))
                  (qss-table-rows table)))))

(defun qss--pad (text width right)
  "Return TEXT padded with spaces to WIDTH columns, on the left if RIGHT."
  (let ((fill (make-string (max 0 (- width (string-width text))) ?\s)))
    (if right (concat fill text) (concat text fill))))

(defun qss--aligned-text (table format)
  "Return TABLE as aligned text; FORMAT is `text', `org' or `md'."
  (let* ((escape (pcase format
                   ('org (lambda (s) (string-replace "|" "\\vert{}" s)))
                   ('md (lambda (s) (string-replace "|" "\\|" s)))
                   (_ #'identity)))
         (cells (mapcar (lambda (line)
                          (mapcar (lambda (s) (funcall escape (qss--one-line s))) line))
                        (qss--text-cells table nil)))
         (rights (mapcar (lambda (spec) (eq (plist-get (cdr spec) :align) 'right))
                         (qss-table-columns table)))
         (widths (apply #'cl-mapcar
                        (lambda (&rest column) (apply #'max 1 (mapcar #'string-width column)))
                        cells))
         (line (lambda (strings)
                 (let ((padded (cl-mapcar #'qss--pad strings widths rights)))
                   (if (eq format 'text)
                       (string-trim-right (mapconcat #'identity padded "  "))
                     (concat "| " (mapconcat #'identity padded " | ") " |")))))
         (rule (pcase format
                 ('org (concat "|-" (mapconcat (lambda (w) (make-string w ?-)) widths "-+-") "-|"))
                 ('md (concat "|" (mapconcat
                                   #'identity
                                   (cl-mapcar (lambda (w right)
                                                (concat (make-string (1+ w) ?-) (if right ":" "-")))
                                              widths rights)
                                   "|")
                              "|")))))
    (concat (funcall line (car cells)) "\n"
            (if rule (concat rule "\n") "")
            (mapconcat (lambda (row) (concat (funcall line row) "\n")) (cdr cells) ""))))

(defun qss--csv-field (text)
  "Return TEXT as a CSV field, quoted if needed."
  (if (string-match-p "[\",\n\r]\\|\\`[ \t]\\|[ \t]\\'" text)
      (concat "\"" (string-replace "\"" "\"\"" text) "\"")
    text))

(cl-defgeneric qss-to (table format)
  "Return TABLE as text in FORMAT, one of `qss-text-formats'.
text, org and md are aligned and show the cells as the table does
\(10k, 2026-10-22 07:11); csv and tsv carry the plain values (sizes in
bytes, durations in nanoseconds, times with seconds), for other
programs.  A query fetches all its rows here.")

(cl-defmethod qss-to (table format)
  "Return TABLE, in memory, as text in FORMAT."
  (let ((table (qss-as-table table))
        (format (if (stringp format) (intern format) format)))
    (pcase format
      ((or 'text 'org 'md) (qss--aligned-text table format))
      ('csv (mapconcat (lambda (line) (concat (mapconcat #'qss--csv-field line ",") "\n"))
                       (qss--text-cells table t) ""))
      ('tsv (mapconcat (lambda (line) (concat (mapconcat #'qss--one-line line "\t") "\n"))
                       (qss--text-cells table t) ""))
      (_ (user-error "Format %S: use one of %s" format
                     (mapconcat #'symbol-name qss-text-formats " "))))))

;;; ------------------------------------------------------------------
;;; qss->: chaining the words

(defvar qss-sources
  '((files . qss-files) (processes . qss-processes) (run . qss-run))
  "Sources that may start a `qss->' pipe without the qss- prefix.")

(defvar qss--words
  '((where . condition) (filter . condition)
    (select . columns) (reject . columns) (get . columns) (group-by . columns)
    (sort-by . columns)
    (first . values) (last . values)
    (count . none) (columns . none) (collect . none)
    (to . columns)
    (each . function))
  "Words of `qss->' and how their arguments are read.")

(defun qss--word-function (word)
  "Return the function that implements WORD.
A word may name its function in its `qss-word-function' property."
  (or (get word 'qss-word-function)
      (qss--word-function-1 word)))

(defun qss--word-function-1 (word)
  "Return the function of WORD by the naming rule: qss-WORD."
  (pcase word
    ('columns 'qss-columns-of)
    ('filter 'qss-where)
    (_ (intern (format "qss-%s" word)))))

(defun qss--expand-source (form)
  "Return FORM with a short source name like `files' expanded."
  (if (and (consp form) (assq (car form) qss-sources))
      (cons (alist-get (car form) qss-sources) (cdr form))
    form))

(defun qss--expand-stage (acc stage)
  "Return code applying STAGE to the code ACC."
  (let* ((stage (if (consp stage) stage (list stage)))
         (word (car stage))
         (args (cdr stage))
         (kind (alist-get word qss--words)))
    (unless kind
      (error "Unknown word in qss->: %S (known: %s)" word
             (mapconcat (lambda (w) (symbol-name (car w))) qss--words " ")))
    (let ((fn (qss--word-function word)))
      (pcase kind
        ('condition `(,fn ,acc ,(qss--condition-code (car args))))
        ('columns `(,fn ,acc ,@(mapcar (lambda (a) `',a) args)))
        ('values `(,fn ,acc ,@args))
        ('none `(,fn ,acc))
        ('function `(,fn ,acc (lambda (it) (ignore it) ,(car args))))))))

(defmacro qss-> (source &rest stages)
  "Pass SOURCE through the words STAGES.
SOURCE is a table, a list, or a source from `qss-sources' such as
\(files \"~\").  Each stage is (WORD ARGUMENTS...) or WORD."
  (let ((acc `(qss-as-input ,(qss--expand-source source))))
    (dolist (stage stages acc)
      (setq acc (qss--expand-stage acc stage)))))

;;; ------------------------------------------------------------------
;;; Command

(defvar qss-eval-history nil
  "History of `qss-eval'.")

;;;###autoload
(defun qss-eval (input)
  "Evaluate the forms in INPUT as with `qss->' and show the result.
Example:  (files \"~\") (where (> :size 100000)) (sort-by size :desc)"
  (interactive (list (read-from-minibuffer "qss: " nil read-expression-map nil
                                           'qss-eval-history)))
  ;; All sources, also when this command was autoloaded on its own.
  (require 'qss)
  (let* ((forms (car (read-from-string (format "(%s)" input))))
         (value (eval `(qss-> ,@forms) t)))
    (qss-display value)
    value))

(provide 'qss-pipe)
;;; qss-pipe.el ends here
