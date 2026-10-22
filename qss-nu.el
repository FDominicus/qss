;;; qss-nu.el --- Nushell as a source for qss  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; Everything Nushell can produce becomes a qss table:
;;
;;   qss> (nu "ps | where cpu > 5")
;;   qss> (nu "open export.csv") (where (> :amount 100)) (sort-by date :desc)
;;   qss> (nu "sys disks")
;;   qss> (nu "ls **/*.el") (sort-by size :desc) (first 10)
;;   qss> (nu "http get https://api.example.org/items")
;;
;; `qss-nu' runs `nu -c "(COMMAND) | to nuon"' and reads the result
;; with qss-nuon.el.  NUON keeps the types of Nushell: sizes, durations
;; and datetimes become columns of the types size, duration and time.
;; With `qss-nu-format' set to `json', JSON is used instead; then only
;; the columns named in `qss-nu-size-columns' are sizes, and durations
;; are plain numbers.
;;   - a list of records becomes the rows,
;;   - a single record becomes rows of `column' and `value',
;;   - a list of plain values becomes rows with the column `value',
;;   - a single plain value is returned as it is.
;; Column names become keys as in qss-sql: "Mount Point" -> mount-point.
;; The command runs in `default-directory', through `process-file', so
;; in a TRAMP buffer it runs on the remote host (if nu is installed there).
;; RET on a row that names a file (as from `ls') opens the file.
;;
;; In the middle of a pipe, `nu' sends the table so far to Nushell, as
;; $in, and takes the result back:
;;
;;   (qss-> (files) (nu "where size > 10kb | math sum"))
;;   (qss-> (sqlite (qss-demo-database) "Orders") (where (is-null :status))
;;          (nu "to csv | save -f open-orders.csv"))
;;
;; Time, size and duration columns arrive in Nushell with their types.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'iso8601)
(require 'qss-sql)
(require 'qss-nuon)

(defcustom qss-nu-program "nu"
  "The Nushell program."
  :type 'string
  :group 'qss)

(defcustom qss-nu-format 'nuon
  "How data goes between Emacs and Nushell.
`nuon' keeps all types of Nushell (sizes, durations, datetimes);
`json' is the fallback for a Nushell whose NUON qss cannot read."
  :type '(choice (const :tag "NUON, with all types" nuon)
                 (const :tag "JSON" json))
  :group 'qss)

(defcustom qss-nu-arguments '("--no-config-file")
  "Arguments for `qss-nu-program' before -c.
The default skips your config.nu and env.nu: faster, and nothing they
print can get into the JSON.  Use nil to have your own commands and
environment."
  :type '(repeat string)
  :group 'qss)

(defcustom qss-nu-size-columns '(size mem virtual total free used available)
  "Columns whose numbers are sizes in bytes, when `qss-nu-format' is `json'.
JSON has no sizes: Nushell writes them as plain numbers.  These columns
are shown as sizes again (363M instead of 380461056).  With NUON the
types are known and this list is not used."
  :type '(repeat symbol)
  :group 'qss)

(defconst qss-nu--datetime-regexp
  (concat "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
          "\\(?:\\.[0-9]+\\)?\\(?:Z\\|[+-][0-9][0-9]:[0-9][0-9]\\)\\'")
  "How Nushell writes a datetime into JSON.")

(defvar qss-nu--dirs (make-hash-table :test #'eq :weakness 'key)
  "Row -> the directory its Nushell command ran in, for RET on files.
Weak, so the entries go away with the rows.")

(defun qss-nu--run (command &optional input)
  "Run the Nushell COMMAND and return its result as JSON text.
COMMAND is complete Nushell code that ends in `to json'.  INPUT, if
given, is text for the standard input of Nushell, available as $in."
  (let ((err-file (make-temp-file "qss-nu-err"))
        (in-file (and input (make-temp-file "qss-nu-in" nil ".json" input)))
        (process-environment (cons "NO_COLOR=1" process-environment))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (with-temp-buffer
          (let ((status (condition-case nil
                            (apply #'process-file qss-nu-program in-file
                                   (list t err-file) nil
                                   (append qss-nu-arguments
                                           (and input '("--stdin"))
                                           (list "-c" command)))
                          (file-missing
                           (user-error "Nushell not found: %s (see `qss-nu-program')"
                                       qss-nu-program)))))
            (unless (eql status 0)
              (user-error "Nushell: %s"
                          (string-trim
                           (with-temp-buffer
                             (insert-file-contents err-file)
                             (buffer-string)))))
            (buffer-string)))
      (delete-file err-file)
      (when in-file (delete-file in-file)))))

(defvar qss-nu--keys (make-hash-table :test #'eq)
  "Column name -> keyword: the same few names come in every row.")

(defun qss-nu--key (name)
  "Return the keyword for the Nushell column NAME (a symbol)."
  (or (gethash name qss-nu--keys)
      (puthash name (intern (format ":%s" (qss-sql-name-key (symbol-name name))))
               qss-nu--keys)))

(defun qss-nu--record-p (v)
  "Return non-nil if V is a JSON object as parsed (an alist with symbol keys)."
  (and (consp v) (consp (car v)) (symbolp (caar v))))

(defun qss-nu--time (s)
  "Return the Nushell datetime text S as a Lisp time."
  (qss-nuon--time s))

(defun qss-nu--convert (v)
  "Convert the parsed JSON value V.
Objects become property lists, datetimes become Lisp times."
  (cond ((qss--time-p v) v)              ; from NUON; may be (TICKS . HZ)
        ((qss-nu--record-p v)
         (cl-mapcan (lambda (cell) (list (qss-nu--key (car cell)) (qss-nu--convert (cdr cell))))
                    v))
        ((consp v) (mapcar #'qss-nu--convert v))
        ((eq v :null) nil)
        ((and (stringp v) (string-match-p qss-nu--datetime-regexp v))
         (condition-case nil (qss-nu--time v) (error v)))
        (t v)))

(defun qss-nu--type (values &optional name)
  "Return the column type of VALUES: `size', `duration', `number', `time' or nil.
All values that are not nil must agree.  NAME is the column name; with
JSON, numbers in a column of `qss-nu-size-columns' are sizes."
  (let ((vs (delq nil (copy-sequence values))))
    (cl-flet ((all (pred) (and vs (cl-every pred vs))))
      (cond ((all #'qss-nuon-size-p) 'size)
            ((all #'qss-nuon-duration-p) 'duration)
            ((all #'numberp)
             (if (and (eq qss-nu-format 'json) (memq name qss-nu-size-columns)) 'size 'number))
            ((all #'qss--time-p) 'time)))))

(defun qss-nu--columns (rows)
  "Return the column specs of ROWS: the union of their keys, in order.
The type of a column comes from its values, see `qss-nu--type'."
  (let ((keys nil))
    (dolist (row rows)
      (cl-loop for (k _v) on row by #'cddr
               unless (memq k keys)
               do (push k keys)))
    (mapcar (lambda (kw)
              (let* ((name (intern (substring (symbol-name kw) 1)))
                     (type (qss-nu--type (mapcar (lambda (r) (plist-get r kw)) rows) name)))
                (if type (list name :type type) name)))
            (nreverse keys))))

(defun qss-nu--parse (text)
  "Return the value of TEXT from Nushell, in `qss-nu-format'.
Nothing is :null; objects are alists with symbol keys."
  (cond ((string-blank-p text) :null)
        ((eq qss-nu-format 'json)
         (json-parse-string text :object-type 'alist :array-type 'list
                            :null-object :null :false-object nil))
        (t (condition-case err
               (qss-nuon-read text)
             (qss-nuon-error
              (error "%s; set `qss-nu-format' to `json' to work around it"
                     (cadr err)))))))

(defun qss-nu--output-code ()
  "Return the Nushell code that writes the result in `qss-nu-format'."
  (if (eq qss-nu-format 'json) "to json --raw" "to nuon"))

(defun qss-nu--display (v)
  "Return V for the value column of a record: times, sizes and durations as text."
  (cond ((qss--time-p v) (qss-format-time v))
        ((qss-nuon-size-p v) (qss-format-size (qss-nuon-size-bytes v)))
        ((qss-nuon-duration-p v) (qss-format-duration (qss-nuon-duration-ns v)))
        (t (qss-nuon-plain v))))

(defun qss-nu (command)
  "Run the Nushell COMMAND and return its result as a table.
See the commentary of qss-nu.el for how the result is turned into rows."
  (let ((dir default-directory))
    (qss-nu--result (qss-nu--run (format "(%s) | %s" command (qss-nu--output-code)))
                    (format "nu %s" command)
                    (lambda () (let ((default-directory dir)) (qss-nu command))))))

(defun qss-nu--result (text title source)
  "Turn the TEXT from Nushell into a table called TITLE with SOURCE.
A single plain value is returned as it is; nothing is nil."
  (let ((dir default-directory)
        (value (qss-nu--parse text)))
    (cond
     ;; nothing (as from save): nil, not an empty table
     ((eq value :null) nil)
     ;; a single record: one row per field
     ((qss-nu--record-p value)
      (qss-make-table title
                      (mapcar (lambda (cell)
                                (list :column (qss-sql-name-key (symbol-name (car cell)))
                                      ;; the value column mixes types: show them as text
                                      :value (qss-nu--display (qss-nu--convert (cdr cell)))))
                              value)
                      '(column value) source))
     ;; a list
     ((listp value)
      (let* ((typed (mapcar #'qss-nu--convert value))
             (records (and typed (consp (car typed)) (keywordp (caar typed))))
             (columns (if records
                          (qss-nu--columns typed)
                        (let ((type (qss-nu--type typed 'value)))
                          (list (if type (list 'value :type type) 'value)))))
             (rows (mapcar #'qss-nuon-plain typed)))
        ;; remember the directory for RET on files
        (dolist (row rows) (when (consp row) (puthash row dir qss-nu--dirs)))
        (qss-make-table title rows columns source)))
     ;; a single plain value
     (t (qss-nuon-plain (qss-nu--convert value))))))

;;; ------------------------------------------------------------------
;;; The other way: a table into Nushell

(defun qss-nu--json-value (v &optional time)
  "Return the Lisp value V as a value for `json-serialize'.
With TIME non-nil, V is a time (the column has the type time) and
becomes a datetime text.  Only the column type decides: a list of two
numbers could be either."
  (cond ((null v) :null)
        ((eq v t) t)
        ((or (and time (qss--time-p v))
             (and (consp v) (integerp (car v)) (integerp (cdr v))))
         (format-time-string "%FT%T.%N%:z" v))
        ((and (consp v) (keywordp (car v)))
         (let ((h (make-hash-table :test #'equal)))
           (cl-loop for (k val) on v by #'cddr
                    do (puthash (substring (symbol-name k) 1) (qss-nu--json-value val) h))
           h))
        ((consp v) (vconcat (mapcar #'qss-nu--json-value v)))
        ((or (stringp v) (numberp v)) v)
        ((symbolp v) (symbol-name v))
        (t (format "%s" v))))

(defun qss-nu--plain-p (table)
  "Return non-nil if the rows of TABLE are single values."
  (let ((rows (qss-table-rows table)))
    (and rows (qss-nuon--plain-row-p (car rows)))))

(defun qss-nu--table-json (table)
  "Return the rows of TABLE as JSON text: objects with the columns of TABLE.
Rows that are single values (as after `get') become plain values."
  (let* ((specs (qss-table-columns table))
         (timep (lambda (c) (eq (plist-get (cdr (assq c specs)) :type) 'time)))
         ;; a single value has the type of the only column
         (value-time (and (= (length specs) 1) (funcall timep (caar specs)))))
    (json-serialize
     (vconcat
      (mapcar (lambda (row)
                (if (qss-nuon--plain-row-p row)
                    (qss-nu--json-value (qss-field row 'value) value-time)
                  (let ((h (make-hash-table :test #'equal)))
                    (dolist (spec specs)
                      (let ((c (car spec)))
                        (puthash (symbol-name c)
                                 (qss-nu--json-value (qss-field row c) (funcall timep c))
                                 h)))
                    h)))
              (qss-table-rows table))))))

(defun qss-nu--input-conversions (table)
  "Return Nushell code to restore the types of TABLE after from json.
Time columns become datetimes, size columns file sizes.  For single
values the type of the only column applies to the whole list."
  (let ((times nil) (sizes nil)
        (specs (qss-table-columns table)))
    (if (qss-nu--plain-p table)
        (pcase (and (= (length specs) 1) (plist-get (cdar specs) :type))
          ('time " | into datetime")
          ('size " | into filesize")
          (_ ""))
      (dolist (spec specs)
        (pcase (plist-get (cdr spec) :type)
          ('time (push (car spec) times))
          ('size (push (car spec) sizes))))
      (concat
       (when times
         (concat " | into datetime "
                 (mapconcat (lambda (c) (format "%S" (symbol-name c))) (nreverse times) " ")))
       (when sizes
         (concat " | into filesize "
                 (mapconcat (lambda (c) (format "%S" (symbol-name c))) (nreverse sizes) " ")))))))

(defun qss-nu-pipe (table command)
  "Send TABLE through the Nushell COMMAND and return the result.
This is the word `nu' in the middle of a pipe:
  (qss-> (files) (nu \"where size > 10kb | math sum\"))
The rows go to Nushell as NUON (or JSON, see `qss-nu-format') with
the types of time, size and duration columns; the result comes back as
from `qss-nu'."
  (let* ((table (qss-as-table table))
         (parent-source (qss-table-source table))
         (dir default-directory))
    (qss-nu--result
     (if (eq qss-nu-format 'json)
         (qss-nu--run (format "$in | from json%s | (%s) | to json --raw"
                              (qss-nu--input-conversions table) command)
                      (qss-nu--table-json table))
       (qss-nu--run (format "$in | from nuon | (%s) | to nuon" command)
                    (qss-nuon-encode-table table)))
     (format "%s | nu %s" (qss-table-title table) command)
     (and parent-source
          (lambda () (let ((default-directory dir))
                       (qss-nu-pipe (funcall parent-source) command)))))))

;; RET on a row from `ls' and the like: open the file or directory.
(cl-defmethod qss-default-action ((row cons))
  "Open the file of ROW if it names one (a :name and a :type of file/dir/symlink).
Otherwise describe ROW."
  (let ((name (and (keywordp (car row)) (plist-get row :name)))
        (type (and (keywordp (car row)) (plist-get row :type))))
    (if (and (stringp name) (member type '("file" "dir" "symlink")))
        (let ((path (expand-file-name name (gethash row qss-nu--dirs default-directory))))
          (if (file-directory-p path)
              (qss-show (qss-files path))
            (find-file path)))
      (qss-describe row))))

(setf (alist-get 'nu qss-sources) 'qss-nu)
(setf (alist-get 'nu qss--words) 'values)
(put 'nu 'qss-word-function 'qss-nu-pipe)

(provide 'qss-nu)
;;; qss-nu.el ends here
