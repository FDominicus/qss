;;; qss-table.el --- Tables as values, shown with vtable  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; Layer 1 of qss, a structured layer for Emacs after the model of Nushell:
;;
;;   - One value: the table (`qss-table').  Every row is an arbitrary
;;     object; `qss-columns' and `qss-field' turn it into a row.  A
;;     process stays a process, a file stays a file.
;;   - Sources: `qss-processes', `qss-files'.
;;   - Display: `qss-show', built on vtable (part of Emacs since 29).
;;
;; Commands:  M-x qss-list-processes
;;            M-x qss-list-files
;;
;; In a table buffer:
;;   RET   default action of the object (open a file, enter a directory,
;;         show a process)
;;   a     menu of the actions for this object
;;   g     read the source again
;;   S     sort by the column at point (vtable)
;;   q     quit the window
;;
;; Own row types: specialize `qss-columns', `qss-field',
;; `qss-default-action' and `qss-actions' with `cl-defmethod'.

;;; Code:

(require 'cl-lib)
(require 'vtable)
(require 'pp)

(defgroup qss nil
  "Tables as values and one vocabulary for all collections."
  :group 'tools
  :prefix "qss-")

(defface qss-table '((t :inherit default))
  "Face of qss tables.
The default is the normal fixed-pitch face.  vtable otherwise uses
`variable-pitch', where digits are wider than the \"x\" vtable measures
widths with, so times and sizes get truncated.  Setting a
proportional font here brings exactly that back."
  :group 'qss)

;;; ------------------------------------------------------------------
;;; The value

(cl-defstruct (qss-table (:constructor qss-table--make)
                         (:copier nil))
  "A table: rows (arbitrary objects) and their columns.
COLUMNS is a list of column specs (see `qss-columns').  SOURCE is a
function of no arguments that builds the table again (for \\`g'), or nil."
  (title "" :type string)
  (columns nil :type list)
  (rows nil :type list)
  (source nil))

(defun qss-make-table (title rows &optional columns source)
  "Return a table called TITLE with ROWS.
Without COLUMNS, the columns of the first row apply (`qss-columns').
SOURCE is a function that rebuilds the table, used by `qss-refresh'."
  (qss-table--make :title title
                   :rows rows
                   :columns (mapcar #'qss--column-spec
                                    (or columns (and rows (qss-columns (car rows)))))
                   :source source))

;;; ------------------------------------------------------------------
;;; What turns an object into a row

(cl-defgeneric qss-columns (row)
  "Return the columns shown for ROW.
A list; each element is a symbol (the field name) or (NAME . PLIST)
with the keys
  :format  function from the raw value to the displayed text
  :align   `left' or `right'
  :type    `number', `size' or `time' (sorting and defaults)
  :width   width in characters.
The column header is the field name; the same name goes to `qss-field'.")

(cl-defgeneric qss-field (row name)
  "Return the raw value of field NAME (a symbol) of ROW.
Filtering and sorting use this value; display goes through the :format
of the column.  Fields need not appear as columns.")

(cl-defgeneric qss-default-action (row)
  "Do what RET does on ROW."
  (qss-describe row))

(cl-defgeneric qss-actions (row)
  "Return the actions for ROW: a list of (LABEL . FUNCTION).
FUNCTION is called with ROW."
  (ignore row)
  (list (cons "Describe" #'qss-describe)))

(cl-defgeneric qss-describe (row)
  "Show all fields of ROW in a help buffer."
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (insert (format "%s\n\n" (type-of row)))
      (dolist (spec (mapcar #'qss--column-spec (qss-columns row)))
        (insert (format "%-12s %s\n" (car spec) (qss--display-value row spec))))
      (insert "\n")
      (pp row (current-buffer)))))

;; Anything else, e.g. numbers or strings: one column `value'.
(cl-defmethod qss-columns (_row)
  "Return the one column of a plain value."
  '(value))

(cl-defmethod qss-field (row _name)
  "Return ROW itself: a plain value has only itself as field."
  row)

;; Property lists with keywords, e.g. rows from SQL:
;; (:pid 1 :name "init") -> columns pid name
(cl-defmethod qss-columns ((row cons))
  "Return the columns of ROW, a property list or an alist."
  (if (keywordp (car row))
      (cl-loop for (k _v) on row by #'cddr
               collect (intern (substring (symbol-name k) 1)))
    (mapcar #'car row)))                ; alist

(cl-defmethod qss-field ((row cons) name)
  "Return field NAME of ROW, a property list or an alist."
  (if (keywordp (car row))
      (plist-get row (intern (format ":%s" name)))
    (alist-get name row)))

;;; ------------------------------------------------------------------
;;; Columns and display

(defun qss--column-spec (spec)
  "Return SPEC as (NAME . PLIST), with defaults depending on :type.
Only missing keys are added, so a spec can pass through here again."
  (let* ((spec (if (consp spec) spec (list spec)))
         (plist (copy-sequence (cdr spec)))
         (type (plist-get plist :type)))
    (unless (plist-member plist :align)
      (setq plist (plist-put plist :align
                             (if (memq type '(number size)) 'right 'left))))
    (unless (plist-member plist :format)
      (setq plist (plist-put plist :format
                             (pcase type
                               ('size #'qss-format-size)
                               ('time #'qss-format-time)
                               (_ #'qss-format-value)))))
    (cons (car spec) plist)))

(defun qss-format-value (value)
  "Return VALUE as text; nil is the empty string."
  (cond ((null value) "")
        ((stringp value) value)
        ;; Floats in full: 39.987 stays 39.987.  To round, give the
        ;; column a :format (as pcpu of processes does).
        ((floatp value) (number-to-string value))
        (t (format "%s" value))))

(defun qss-format-percent (value)
  "Return the percentage VALUE with one decimal."
  (if (numberp value) (format "%.1f" value) ""))

(defun qss-format-size (bytes)
  "Return BYTES in short form: 1023, 10k, 3.2M."
  (cond ((null bytes) "")
        ((< bytes 1024) (format "%d" bytes))
        (t (file-size-human-readable bytes))))

(defun qss-format-time (time)
  "Return TIME (a Lisp time or seconds) as 2026-10-22 07:11."
  (if time (format-time-string "%Y-%m-%d %H:%M" time) ""))

(defun qss--sort-value (row spec)
  "Return the sort value of ROW in column SPEC, for vtable.
Numbers stay numbers, times become seconds, the rest is text."
  (let ((v (qss-field row (car spec)))
        (type (plist-get (cdr spec) :type)))
    (pcase type
      ((or 'number 'size) (if (numberp v) v 0))
      ('time (if v (float-time v) 0))
      (_ (qss-format-value v)))))

(defun qss--display-value (row spec)
  "Return the displayed text of ROW in column SPEC."
  (funcall (plist-get (cdr spec) :format) (qss-field row (car spec))))

;;; ------------------------------------------------------------------
;;; Display

(defvar-local qss--table nil "The table this buffer shows.")

(defvar-local qss--vtable-buffer nil
  "Non-nil in buffers where qss inserts vtables (for the header fix).")

(defun qss--fix-tty-header (_table widths spacer)
  "Align the vtable header on a text terminal (Emacs 29.3).
When padding a header cell, vtable subtracts the column spacing and
half a character, and on a terminal it computes in whole characters:
every header cell becomes too narrow and short columns run together.
Here every cell is set to WIDTHS[i] plus SPACER, like the rows below.
Only in buffers where qss inserts tables and only without graphics."
  (when (and qss--vtable-buffer (not (display-graphic-p)))
    (let* ((inhibit-read-only t)
           (end (1- (point)))                     ; before the newline
           (pos (save-excursion (forward-line -1) (point)))
           (last (1- (length widths))))
      (while (< pos end)
        (let* ((index (get-text-property pos 'vtable-column))
               (cend (next-single-property-change pos 'vtable-column nil end))
               (spaces nil) (text 0))
          ;; In the cell: name, padding, sort indicator, half spacing.
          (let ((p pos))
            (while (< p cend)
              (let ((n (next-single-property-change p 'display nil cend)))
                (if (eq 'space (car-safe (get-text-property p 'display)))
                    (push p spaces)
                  (cl-incf text (string-width (buffer-substring-no-properties p n))))
                (setq p n))))
          (setq spaces (nreverse spaces))
          (when (and index spaces)
            (let ((fill (max 1 (- (+ (elt widths index) (if (= index last) 0 spacer))
                                  text))))
              (put-text-property (car spaces) (1+ (car spaces))
                                 'display `(space :width (,fill)))
              (dolist (s (cdr spaces))
                (put-text-property s (1+ s) 'display '(space :width (0))))))
          (setq pos cend))))))

(advice-add 'vtable--insert-header-line :after #'qss--fix-tty-header)

(defvar qss-table-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    (define-key m (kbd "g") #'qss-refresh)
    m)
  "Keymap of `qss-table-mode'.")

(define-derived-mode qss-table-mode special-mode "qss-table"
  "Buffer showing a table of objects.
\\{qss-table-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (qss-refresh))))

(defcustom qss-max-column-width 60
  "Maximum width of a text column without its own :width, in characters."
  :type 'integer
  :group 'qss)

(defun qss--column-width (table spec)
  "Return the width of SPEC in TABLE, from the displayed text of all rows."
  (or (plist-get (cdr spec) :width)
      (min qss-max-column-width
           (apply #'max (string-width (symbol-name (car spec)))
                  (mapcar (lambda (row) (string-width (qss--display-value row spec)))
                          (qss-table-rows table))))))

(defun qss--vtable-columns (table)
  "Return the vtable columns for TABLE.
The getter returns the sort value: numbers for numbers, sizes and
times, otherwise the displayed text, already cut to the column width.
The width is computed here and given to vtable, because vtable 29.3
measures it on the raw value (seconds instead of the date) and measures
every text in full (command lines with tens of thousands of characters)."
  (mapcar (lambda (spec)
            (let* ((type (plist-get (cdr spec) :type))
                   (fmt (plist-get (cdr spec) :format))
                   (width (qss--column-width table spec))
                   (numeric (memq type '(number size time))))
              (list :name (symbol-name (car spec))
                    :align (plist-get (cdr spec) :align)
                    :width width
                    :getter (if numeric
                                (lambda (row _vt) (qss--sort-value row spec))
                              (lambda (row _vt)
                                (truncate-string-to-width
                                 (qss--display-value row spec) width nil nil t)))
                    :formatter (pcase type
                                 ('time (lambda (secs)
                                          (if (zerop secs) "" (funcall fmt secs))))
                                 ((or 'number 'size) fmt)
                                 (_ #'identity)))))
          (qss-table-columns table)))

(defun qss-insert-vtable (table &optional keymap)
  "Insert the rows of TABLE at point as a vtable.
RET runs the default action of a row, \`a' offers its actions.
KEYMAP, if given, is merged into the keymap of the table rows."
  (setq qss--vtable-buffer t)
  (if (null (qss-table-rows table))
      (insert "(no rows)\n")
    (make-vtable
     :objects (qss-table-rows table)
     :columns (qss--vtable-columns table)
     ;; Header in the buffer, not in the header line: on a terminal the
     ;; vtable header line is misaligned (Emacs 29.3).
     :use-header-line nil
     :face 'qss-table
     :keymap keymap
     :actions '("RET" qss--default "a" qss--actions-menu))))

(defun qss--insert-table (table)
  "Insert TABLE into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (qss-table-title table) 'face 'bold)
            (format "  (%d)\n\n" (length (qss-table-rows table))))
    ;; g on a row: read again, not only redraw (vtable binds g to
    ;; `vtable-revert-command' otherwise).
    (qss-insert-vtable table (define-keymap "g" #'qss-refresh))))

(defun qss-show (table)
  "Show TABLE in a buffer of its own and return the buffer."
  (let ((buf (get-buffer-create (format "*qss %s*" (qss-table-title table)))))
    (with-current-buffer buf
      (qss-table-mode)
      (setq qss--table table)
      (qss--insert-table table)
      (goto-char (point-min))
      (forward-line 2))
    (pop-to-buffer buf)
    buf))

(defun qss-refresh ()
  "Read the table of this buffer again, keeping roughly the same line."
  (interactive)
  (let ((source (and qss--table (qss-table-source qss--table)))
        (line (line-number-at-pos)))
    (unless source (user-error "This table has no source"))
    (setq qss--table (funcall source))
    (qss--insert-table qss--table)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun qss-current-row ()
  "Return the object of the row at point."
  (or (vtable-current-object) (user-error "No row here")))

(defun qss--default (row)
  "Run the default action of ROW."
  (qss-default-action row))

(defun qss--actions-menu (row)
  "Choose one of the actions of ROW and run it."
  (let* ((actions (qss-actions row))
         (choice (completing-read "Action: " (mapcar #'car actions) nil t)))
    (funcall (cdr (assoc choice actions)) row)))

;;; ------------------------------------------------------------------
;;; Source: processes

(cl-defstruct (qss-process (:constructor qss-process--make) (:copier nil))
  "A system process: PID and the attributes from `process-attributes'."
  pid attrs)

(defun qss-processes ()
  "Return a table of the running processes (from `process-attributes')."
  (qss-make-table
   "processes"
   (delq nil (mapcar (lambda (pid)
                       (let ((attrs (process-attributes pid)))
                         (and attrs (qss-process--make :pid pid :attrs attrs))))
                     (list-system-processes)))
   nil #'qss-processes))

(cl-defmethod qss-columns ((_row qss-process))
  "Return the columns of a process row."
  '((pid :type number) user (state :width 5)
    (pcpu :type number :format qss-format-percent)
    (pmem :type number :format qss-format-percent)
    (rss :type size) (start :type time) (command :width 60)))

(cl-defmethod qss-field ((row qss-process) name)
  "Return field NAME of the process ROW; rss is in bytes."
  (let ((a (qss-process-attrs row)))
    (pcase name
      ('pid (qss-process-pid row))
      ('rss (let ((kb (alist-get 'rss a))) (and kb (* 1024 kb))))  ; KB -> bytes
      ('command (or (alist-get 'args a)
                    (format "[%s]" (alist-get 'comm a))))
      (_ (alist-get name a)))))

(cl-defmethod qss-default-action ((row qss-process))
  "Describe the process ROW."
  (qss-describe row))

(cl-defmethod qss-actions ((row qss-process))
  "Return the actions for the process ROW."
  (ignore row)
  (list (cons "Describe" #'qss-describe)
        (cons "Terminate (TERM)" (lambda (r) (qss--signal r 'SIGTERM)))
        (cons "Kill (KILL)" (lambda (r) (qss--signal r 'SIGKILL)))))

(defun qss--signal (row sig)
  "Send signal SIG to the process of ROW, after asking."
  (let ((pid (qss-process-pid row)))
    (when (yes-or-no-p (format "Send %s to %d (%s)? " sig pid (qss-field row 'command)))
      (signal-process pid sig)
      (message "Sent %s to %d" sig pid))))

;;; ------------------------------------------------------------------
;;; Source: files

(cl-defstruct (qss-file (:constructor qss-file--make) (:copier nil))
  "A file: its PATH and the attributes from `file-attributes'."
  path attrs)

(defun qss-files (&optional dir)
  "Return a table of the files in DIR (default `default-directory').
Works with TRAMP paths too."
  (let ((dir (file-name-as-directory (expand-file-name (or dir default-directory)))))
    (qss-make-table
     (format "files %s" (abbreviate-file-name dir))
     (cl-loop for (path . attrs) in (directory-files-and-attributes dir t nil t 'string)
              unless (member (file-name-nondirectory path) '("." ".."))
              collect (qss-file--make :path path :attrs attrs))
     (qss-columns (qss-file--make))
     (lambda () (qss-files dir)))))

(cl-defmethod qss-columns ((_row qss-file))
  "Return the columns of a file row."
  '((name :width 40) type (size :type size) (modified :type time) mode user))

(cl-defmethod qss-field ((row qss-file) name)
  "Return field NAME of the file ROW."
  (let ((a (qss-file-attrs row))
        (path (qss-file-path row)))
    (pcase name
      ('name (file-name-nondirectory path))
      ('path path)
      ('dir-p (eq t (file-attribute-type a)))
      ('type (let ((ty (file-attribute-type a)))
               (cond ((eq ty t) "dir") ((stringp ty) "link") (t "file"))))
      ('target (let ((ty (file-attribute-type a))) (and (stringp ty) ty)))
      ('size (file-attribute-size a))
      ('modified (file-attribute-modification-time a))
      ('mode (file-attribute-modes a))
      ('user (file-attribute-user-id a))
      ('group (file-attribute-group-id a)))))

(cl-defmethod qss-default-action ((row qss-file))
  "Visit the file ROW, or show the directory ROW as a table."
  (if (qss-field row 'dir-p)
      (qss-show (qss-files (qss-file-path row)))
    (find-file (qss-file-path row))))

(cl-defmethod qss-actions ((row qss-file))
  "Return the actions for the file ROW."
  (ignore row)
  (list (cons "Open" #'qss-default-action)
        (cons "Dired" (lambda (r) (dired-jump nil (qss-file-path r))))
        (cons "Copy path" (lambda (r) (kill-new (qss-file-path r))
                            (message "%s" (qss-file-path r))))
        (cons "Describe" #'qss-describe)
        (cons "Delete" #'qss--delete-file)))

(defun qss--delete-file (row)
  "Delete the file of ROW after asking, then read the table again."
  (let ((path (qss-file-path row)))
    (when (yes-or-no-p (format "Delete %s? " path))
      (if (qss-field row 'dir-p)
          (delete-directory path t)
        (delete-file path))
      (qss-refresh))))

;;; ------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun qss-list-processes ()
  "Show the running processes as a table."
  (interactive)
  (qss-show (qss-processes)))

;;;###autoload
(defun qss-list-files (dir)
  "Show the files in DIR as a table."
  (interactive "DDirectory: ")
  (qss-show (qss-files dir)))

(provide 'qss-table)
;;; qss-table.el ends here
