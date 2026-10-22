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
;; `qss-nu' runs `nu -c "(COMMAND) | to json --raw"' and reads the JSON:
;;   - a list of records becomes the rows,
;;   - a single record becomes rows of `column' and `value',
;;   - a list of plain values becomes rows with the column `value',
;;   - a single plain value is returned as it is.
;; Column names become keys as in qss-sql: "Mount Point" -> mount-point.
;; The command runs in `default-directory', through `process-file', so
;; in a TRAMP buffer it runs on the remote host (if nu is installed there).
;; RET on a row that names a file (as from `ls') opens the file.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'iso8601)
(require 'qss-sql)

(defcustom qss-nu-program "nu"
  "The Nushell program."
  :type 'string
  :group 'qss)

(defcustom qss-nu-arguments '("--no-config-file")
  "Arguments for `qss-nu-program' before -c.
The default skips your config.nu and env.nu: faster, and nothing they
print can get into the JSON.  Use nil to have your own commands and
environment."
  :type '(repeat string)
  :group 'qss)

(defcustom qss-nu-size-columns '(size mem virtual total free used available)
  "Columns whose numbers are sizes in bytes.
Nushell writes file sizes into JSON as plain numbers; these columns are
shown as sizes again (363M instead of 380461056)."
  :type '(repeat symbol)
  :group 'qss)

(defconst qss-nu--datetime-regexp
  (concat "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
          "\\(?:\\.[0-9]+\\)?\\(?:Z\\|[+-][0-9][0-9]:[0-9][0-9]\\)\\'")
  "How Nushell writes a datetime into JSON.")

(defvar qss-nu--dirs (make-hash-table :test #'eq :weakness 'key)
  "Row -> the directory its Nushell command ran in, for RET on files.
Weak, so the entries go away with the rows.")

(defun qss-nu--run (command)
  "Run the Nushell COMMAND and return its result as JSON text."
  (let ((err-file (make-temp-file "qss-nu-err"))
        (process-environment (cons "NO_COLOR=1" process-environment))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (with-temp-buffer
          (let ((status (condition-case nil
                            (apply #'process-file qss-nu-program nil
                                   (list t err-file) nil
                                   (append qss-nu-arguments
                                           (list "-c" (format "(%s) | to json --raw" command))))
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
      (delete-file err-file))))

(defun qss-nu--key (name)
  "Return the keyword for the Nushell column NAME (a symbol)."
  (intern (format ":%s" (qss-sql-name-key (symbol-name name)))))

(defun qss-nu--record-p (v)
  "Return non-nil if V is a JSON object as parsed (an alist with symbol keys)."
  (and (consp v) (consp (car v)) (symbolp (caar v))))

(defun qss-nu--time (s)
  "Return the Nushell datetime text S as a Lisp time."
  ;; iso8601 takes at most 9 fraction digits; Nushell writes nanoseconds.
  (encode-time (iso8601-parse s)))

(defun qss-nu--convert (v)
  "Convert the parsed JSON value V.
Objects become property lists, datetimes become Lisp times."
  (cond ((qss-nu--record-p v)
         (cl-mapcan (lambda (cell) (list (qss-nu--key (car cell)) (qss-nu--convert (cdr cell))))
                    v))
        ((consp v) (mapcar #'qss-nu--convert v))
        ((and (stringp v) (string-match-p qss-nu--datetime-regexp v))
         (condition-case nil (qss-nu--time v) (error v)))
        (t v)))

(defun qss-nu--columns (rows)
  "Return the column specs of ROWS: the union of their keys, in order.
A column is a number column if all its values are numbers, a time
column if all are times; numbers in a column of
`qss-nu-size-columns' are sizes in bytes."
  (let ((keys nil))
    (dolist (row rows)
      (cl-loop for (k _v) on row by #'cddr
               unless (memq k keys)
               do (push k keys)))
    (mapcar (lambda (kw)
              (let ((name (intern (substring (symbol-name kw) 1))))
                (cl-flet ((all (pred)
                            (and (cl-some (lambda (r) (plist-get r kw)) rows)
                                 (cl-every (lambda (r) (let ((v (plist-get r kw)))
                                                         (or (null v) (funcall pred v))))
                                           rows))))
                  (cond ((all #'numberp)
                         (list name :type (if (memq name qss-nu-size-columns) 'size 'number)))
                        ((all #'qss--time-p) (list name :type 'time))
                        (t name)))))
          (nreverse keys))))

(defun qss-nu (command)
  "Run the Nushell COMMAND and return its result as a table.
See the commentary of qss-nu.el for how the result is turned into rows."
  (let* ((dir default-directory)
         (json (qss-nu--run command))
         (value (if (string-blank-p json)
                    nil
                  (json-parse-string json :object-type 'alist :array-type 'list
                                     :null-object nil :false-object nil)))
         (title (format "nu %s" command))
         (source (lambda () (let ((default-directory dir)) (qss-nu command)))))
    (cond
     ;; a single record: one row per field
     ((qss-nu--record-p value)
      (qss-make-table title
                      (mapcar (lambda (cell)
                                (let ((v (qss-nu--convert (cdr cell))))
                                  (list :column (qss-sql-name-key (symbol-name (car cell)))
                                        ;; the value column mixes types: show times readably
                                        :value (if (qss--time-p v) (qss-format-time v) v))))
                              value)
                      '(column value) source))
     ;; a list
     ((listp value)
      (let ((rows (mapcar #'qss-nu--convert value)))
        ;; remember the directory for RET on files
        (dolist (row rows) (when (consp row) (puthash row dir qss-nu--dirs)))
        (qss-make-table title rows
                        (if (and rows (consp (car rows)) (keywordp (caar rows)))
                            (qss-nu--columns rows)
                          '(value))
                        source)))
     ;; a single plain value
     (t value))))

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

(provide 'qss-nu)
;;; qss-nu.el ends here
