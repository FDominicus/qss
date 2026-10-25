;;; qss-present.el --- Presentations and accept, after CLIM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; What is shown stays the object it shows: a presentation is a piece
;; of text together with an object and a presentation type.  Every row
;; of a qss table is a presentation of its row object (a file, a
;; process ...), every cell a presentation of its value (a size, a
;; time, a string ...).  `qss-present' makes one anywhere else.
;;
;; `qss-accept' is the other half.  It asks for an object of a type:
;;
;;   (qss-accept 'directory "Copy into: ")
;;
;; While it asks, every visible presentation of that type is
;; highlighted; a click on one (or RET on it) is the answer, and the
;; answer is the object itself, not its text.  Typing works too, with
;; completion over the highlighted ones, and text is parsed for the
;; type: a path for `file', "10kb" for `size', "2026-10-22" for `time'.
;;
;; Types form a hierarchy (`qss-define-presentation-type'):
;;
;;   t
;;   ├─ object ─ file ─ directory, process, db-table, row
;;   └─ value ─ number ─ integer ─ size, duration
;;            ├─ string, time, symbol
;;
;; A directory is also a file, a size also a number: accept `file' and
;; directories are offered as well.
;;
;; Commands with typed arguments (`qss-define-command') bring both
;; halves together.  `qss-compare-files' takes two files; on a file
;; row, `a' offers it with that file as the first argument and accepts
;; the second, by a click on another row.  M-x works too and accepts
;; both.

;;; Code:

(require 'cl-lib)
(require 'qss-sql)

(defface qss-accept '((t :inherit highlight))
  "Face of the presentations `qss-accept' offers."
  :group 'qss)

;;; ------------------------------------------------------------------
;;; Types

(defvar qss--presentation-types (make-hash-table :test #'eq)
  "Presentation type -> plist (:supertype TYPE :parser FN :doc TEXT).")

(defun qss-define-presentation-type (type supertype &optional parser doc)
  "Define the presentation TYPE as a subtype of SUPERTYPE.
PARSER, if given, turns typed text into an object of TYPE or returns
nil; a type without one uses the parser of its supertype.  DOC
describes the type."
  (puthash type (list :supertype supertype :parser parser :doc doc)
           qss--presentation-types)
  type)

(defun qss-presentation-type-p (type)
  "Return non-nil if TYPE is a presentation type (or t)."
  (or (eq type t) (and (symbolp type) (gethash type qss--presentation-types) t)))

(defun qss-presentation-supertype (type)
  "Return the supertype of TYPE, or nil for t."
  (plist-get (gethash type qss--presentation-types) :supertype))

(defun qss-presentation-subtype-p (type supertype)
  "Return non-nil if TYPE is SUPERTYPE or below it."
  (let ((ty type) (found nil))
    (while (and ty (not found))
      (if (eq ty supertype) (setq found t) (setq ty (qss-presentation-supertype ty))))
    (or found (eq supertype t))))

(defun qss--parse-number (text)
  "Return TEXT as a number, or nil."
  (and (string-match-p "\\`[ \t]*-?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?[ \t]*\\'" text)
       (string-to-number text)))

(defun qss--parse-file (text)
  "Return TEXT as the name of an existing file, or nil."
  (let ((f (expand-file-name (substitute-in-file-name text))))
    (and (file-exists-p f) f)))

(defun qss--parse-quantity-of (type)
  "Return a parser for sizes or durations (TYPE) as Nushell writes them."
  (lambda (text)
    (let ((q (qss-parse-quantity (string-trim text))))
      (cond ((and q (eq (car q) type)) (cdr q))
            ((qss--parse-number text))))))

(qss-define-presentation-type 'object t nil "Something with actions: a row object.")
(qss-define-presentation-type 'file 'object #'qss--parse-file "A file or directory.")
(qss-define-presentation-type 'directory 'file
                              (lambda (text) (let ((f (qss--parse-file text)))
                                               (and f (file-directory-p f) (file-name-as-directory f))))
                              "A directory.")
(qss-define-presentation-type 'process 'object
                              (lambda (text) (let ((n (qss--parse-number text)))
                                               (and (integerp n) (process-attributes n)
                                                    (car (qss-table-rows
                                                          (qss-> (processes) (where (= :pid n))))))))
                              "A system process.")
(qss-define-presentation-type 'db-table 'object nil "A table of an SQLite file.")
(qss-define-presentation-type 'line 'object nil "A line of a file (`lines').")
(qss-define-presentation-type 'row 'object nil "A row of data (a property list).")
(qss-define-presentation-type 'value t
                              (lambda (text) (or (qss--parse-number text) text))
                              "A plain value: number, text, time.")
(qss-define-presentation-type 'number 'value #'qss--parse-number "A number.")
(qss-define-presentation-type 'integer 'number
                              (lambda (text) (let ((n (qss--parse-number text))) (and (integerp n) n)))
                              "A whole number.")
(qss-define-presentation-type 'size 'integer (qss--parse-quantity-of 'size)
                              "A size in bytes; \"10kb\" can be typed.")
(qss-define-presentation-type 'duration 'integer (qss--parse-quantity-of 'duration)
                              "A duration in nanoseconds; \"3hr\" can be typed.")
(qss-define-presentation-type 'string 'value #'identity "Text.")
(qss-define-presentation-type 'symbol 'value #'intern "A symbol.")
(qss-define-presentation-type 'time 'value
                              (lambda (text) (ignore-errors (qss--to-time (string-trim text))))
                              "A point in time; \"2026-10-22\" or \"2026-10-22 07:30\" can be typed.")

(defun qss-presentation-parse (type text)
  "Return TEXT as an object of TYPE, or nil if it is not one.
The parser of TYPE or of the nearest supertype that has one is used."
  (let ((ty type) (parser nil))
    (while (and ty (not parser))
      (setq parser (plist-get (gethash ty qss--presentation-types) :parser)
            ty (qss-presentation-supertype ty)))
    (and parser (funcall parser text))))

;;; ------------------------------------------------------------------
;;; The type and label of an object

(cl-defgeneric qss-presentation-type (object)
  "Return the presentation type of OBJECT, a symbol.")

(cl-defmethod qss-presentation-type (object)
  "Return the type of a plain value, or t."
  (cond ((integerp object) 'integer)
        ((numberp object) 'number)
        ((stringp object) 'string)
        ((qss--time-p object) 'time)
        ((and object (symbolp object)) 'symbol)
        (t t)))

(cl-defmethod qss-presentation-type ((object cons))
  "Return the type of a list: a time, a row, or a file if the row names one."
  (cond ((qss--time-p object) 'time)
        ((keywordp (car object))
         (let ((path (qss--row-path object)))
           (cond ((null path) 'row)
                 ((file-directory-p path) 'directory)
                 (t 'file))))
        (t t)))

(cl-defmethod qss-presentation-type ((object qss-file))
  "A file row is a `file' or a `directory'."
  (if (qss-field object 'dir-p) 'directory 'file))

(cl-defmethod qss-presentation-type ((_object qss-process))
  "A process row is a `process'."
  'process)

(cl-defmethod qss-presentation-type ((_object qss-line))
  "A row of `lines' is a `line'."
  'line)

(cl-defmethod qss-presentation-label ((object qss-line))
  "Return the line row as FILE:NUMBER."
  (format "%s:%d" (file-name-nondirectory (qss-line-file object)) (qss-line-number object)))

(cl-defmethod qss-presentation-type ((_object qss-db-table))
  "A row of `tables' is a `db-table'."
  'db-table)

(cl-defmethod qss-presentation-label ((object qss-db-table))
  "Return the table row as its name."
  (qss-db-table-name object))

(cl-defgeneric qss-presentation-label (object)
  "Return a short text for OBJECT, for completion in `qss-accept'.")

(cl-defmethod qss-presentation-label (object)
  "Return OBJECT as plain text."
  (qss--plain-value object nil))

(cl-defmethod qss-presentation-label ((object cons))
  "Return a row as its path or its first two fields, a time as text."
  (cond ((qss--time-p object) (qss--plain-value object nil))
        ((qss--row-path object) (abbreviate-file-name (qss--row-path object)))
        ((keywordp (car object))
         (mapconcat (lambda (v) (qss--plain-value v nil))
                    (cl-loop for (_k v) on object by #'cddr repeat 2 collect v)
                    " "))
        (t (format "%S" object))))

(cl-defmethod qss-presentation-label ((object qss-file))
  "Return the file row as its abbreviated path."
  (abbreviate-file-name (qss-file-path object)))

(cl-defmethod qss-presentation-label ((object qss-process))
  "Return the process row as PID and command."
  (truncate-string-to-width (format "%d %s" (qss-process-pid object) (qss-field object 'command))
                            60))

(cl-defgeneric qss-file-name (object)
  "Return the file name OBJECT stands for.
A file row, a stored row with a path, or a name; own row types whose
presentation type is `file' add a method, so the commands for files
work on them.")

(cl-defmethod qss-file-name (object)
  "Return the name of the file OBJECT, a name or a row with a path."
  (cond ((stringp object) (expand-file-name object))
        ((qss--row-path object))
        (t (user-error "Not a file: %S" object))))

(cl-defmethod qss-file-name ((object qss-file))
  "Return the path of the file row OBJECT."
  (qss-file-path object))

;;; ------------------------------------------------------------------
;;; Presenting

(defun qss-present (object &optional type text)
  "Insert TEXT (default the label of OBJECT) as a presentation of OBJECT.
TYPE defaults to `qss-presentation-type' of OBJECT.  `qss-accept' can
then offer it."
  (insert (propertize (or text (qss-presentation-label object))
                      'qss-presentation (cons object (or type (qss-presentation-type object)))
                      'mouse-face 'highlight
                      'help-echo (format "%s" (or type (qss-presentation-type object))))))

(defun qss--cell-type (spec value)
  "Return the presentation type of VALUE in the column SPEC."
  (or (and value (memq (plist-get (cdr spec) :type) '(size duration time))
           (plist-get (cdr spec) :type))
      (and value (qss-presentation-type value))))

(defun qss--presentations-in (start end type)
  "Return the presentations of TYPE between START and END in this buffer.
A list of (BEG END OBJECT TYPE).  A row whose object is of TYPE is one
presentation; otherwise each cell whose value is of TYPE is one."
  (let ((found nil) (pos start))
    (while (< pos end)
      (let* ((row (get-text-property pos 'vtable-object))
             (pres (get-text-property pos 'qss-presentation))
             (next (min end (next-single-property-change
                             pos (cond (row 'vtable-object) (pres 'qss-presentation) (t 'vtable-object))
                             nil end)))
             (next (if (or row pres) next
                     (min next (next-single-property-change pos 'qss-presentation nil end)))))
        (cond
         (row
          (let ((row-type (qss-presentation-type row)))
            (if (and (not (eq row-type t)) (qss-presentation-subtype-p row-type type))
                (push (list pos (save-excursion (goto-char pos) (line-end-position)) row row-type) found)
              ;; the cells of the row
              (let* ((vt (get-text-property pos 'vtable))
                     (whole (and vt qss--whole-tables (gethash vt qss--whole-tables)))
                     (p pos))
                (while (< p next)
                  (let ((index (get-text-property p 'vtable-column))
                        (cend (min next (next-single-property-change p 'vtable-column nil next))))
                    (when (and index vt)
                      (let* ((name (intern (vtable-column vt index)))
                             (spec (if (qss-table-p whole) (qss--spec-of whole name)
                                     (qss--column-spec name)))
                             (value (qss-field row name))
                             (cell-type (qss--cell-type spec value)))
                        (when (and cell-type (not (eq cell-type t))
                                   (qss-presentation-subtype-p cell-type type))
                          ;; only the text, not the padding around it
                          (let* ((tbeg (save-excursion
                                         (goto-char p) (skip-chars-forward " \t" cend) (point)))
                                 (tend (save-excursion
                                         (goto-char cend) (skip-chars-backward " \t\n" tbeg) (point))))
                            (when (< tbeg tend)
                              (push (list tbeg tend value cell-type) found))))))
                    (setq p cend)))))))
         (pres
          (when (qss-presentation-subtype-p (cdr pres) type)
            (push (list pos next (car pres) (cdr pres)) found))))
        (setq pos next)))
    (nreverse found)))

(defun qss--visible-presentations (type)
  "Return the presentations of TYPE in all visible windows.
A list of (WINDOW BEG END OBJECT TYPE)."
  (let ((found nil))
    (dolist (w (window-list-1 nil 'nomini 'visible))
      (with-current-buffer (window-buffer w)
        (dolist (p (qss--presentations-in (window-start w) (window-end w t) type))
          (push (cons w p) found))))
    (nreverse found)))

;;; ------------------------------------------------------------------
;;; Accepting

(defvar qss--accept-chosen nil
  "The object chosen by a click while `qss-accept' asks, wrapped in a list.")

(defvar qss--accept-overlays nil
  "The overlays `qss-accept' shows.")

(defun qss--accept-choose (overlay)
  "Choose the object of OVERLAY and end the question of `qss-accept'."
  (setq qss--accept-chosen (list (overlay-get overlay 'qss-object)))
  (when (active-minibuffer-window)
    (exit-minibuffer)))

(defun qss--accept-overlay-at (pos)
  "Return the overlay of `qss-accept' at POS in the current buffer, or nil."
  (cl-find-if (lambda (o) (overlay-get o 'qss-object)) (overlays-at pos)))

(defun qss-accept-click (event)
  "Choose the presentation clicked on with EVENT."
  (interactive "e")
  (let* ((posn (event-start event))
         (overlay (with-current-buffer (window-buffer (posn-window posn))
                    (qss--accept-overlay-at (posn-point posn)))))
    (if overlay
        (qss--accept-choose overlay)
      (user-error "Not one of the highlighted ones"))))

(defun qss-accept-at-point ()
  "Choose the presentation at point."
  (interactive)
  (let ((overlay (qss--accept-overlay-at (point))))
    (if overlay
        (qss--accept-choose overlay)
      (user-error "Not one of the highlighted ones"))))

(defvar qss-accept-map
  (define-keymap
    "<down-mouse-1>" #'ignore
    "<mouse-1>" #'qss-accept-click
    "<mouse-2>" #'qss-accept-click
    "RET" #'qss-accept-at-point)
  "Keymap on the presentations `qss-accept' offers.")

(defun qss--accept-highlight (presentations)
  "Show PRESENTATIONS, a list as from `qss--visible-presentations'."
  (dolist (p presentations)
    (pcase-let ((`(,w ,beg ,end ,object ,type) p))
      (let ((o (make-overlay beg end (window-buffer w))))
        (overlay-put o 'face 'qss-accept)
        (overlay-put o 'mouse-face 'highlight)
        (overlay-put o 'keymap qss-accept-map)
        (overlay-put o 'priority 100)
        (overlay-put o 'help-echo (format "%s: click to choose" type))
        (overlay-put o 'qss-object object)
        (push o qss--accept-overlays)))))

(defun qss--accept-labels (presentations)
  "Return an alist (LABEL . OBJECT) for PRESENTATIONS, labels unique."
  (let ((seen (make-hash-table :test #'equal)) (result nil))
    (dolist (p presentations)
      (let* ((object (nth 3 p))
             (label (qss-presentation-label object)))
        (unless (gethash label seen)
          (puthash label t seen)
          (push (cons label object) result))))
    (nreverse result)))

(defun qss-accept (type &optional prompt)
  "Ask for an object of the presentation TYPE and return it.
All visible presentations of TYPE are highlighted: a click on one, or
RET on one, chooses it.  Typed text is completed over their labels; if
it is none of them, it is parsed for TYPE (`qss-presentation-parse').
PROMPT defaults to the name of the type."
  (unless (gethash type qss--presentation-types)
    (unless (eq type t) (error "Unknown presentation type: %S" type)))
  (let* ((presentations (qss--visible-presentations type))
         (labels (qss--accept-labels presentations))
         (prompt (or prompt (format "%s: " (capitalize (symbol-name type)))))
         (prompt (if presentations
                     (format "%s(%d shown, click one or type) " prompt (length labels))
                   prompt)))
    (setq qss--accept-chosen nil)
    (unwind-protect
        (progn
          (qss--accept-highlight presentations)
          (let ((text (completing-read prompt (qss--ordered-labels (mapcar #'car labels)))))
            (cond (qss--accept-chosen (car qss--accept-chosen))
                  ((assoc text labels) (cdr (assoc text labels)))
                  ((string-empty-p text) (user-error "Nothing chosen"))
                  ((qss-presentation-parse type text))
                  (t (user-error "Not a %s: %s" type text)))))
      (mapc #'delete-overlay qss--accept-overlays)
      (setq qss--accept-overlays nil))))

(defun qss--ordered-labels (labels)
  "Return a completion table of LABELS that keeps their order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity) (cycle-sort-function . identity))
      (complete-with-action action labels string pred))))

;;; ------------------------------------------------------------------
;;; Commands with typed arguments

(defvar qss-commands nil
  "Commands with typed arguments: a list of (NAME TITLE ARGS).
ARGS is a list of (ARG TYPE PROMPT).  `qss-define-command' adds to it;
the actions at point offer a command when the object or value there is
of the type of its first argument.")

(defmacro qss-define-command (name title args docstring &rest body)
  "Define NAME, a command whose arguments are accepted by type.
TITLE is its label in the actions at point.  ARGS is a list of
\(ARG TYPE PROMPT).  Called with M-x, every argument is accepted with
`qss-accept'; offered on an object of the type of the first argument,
that object is the first argument and the rest are accepted."
  (declare (indent 3) (doc-string 4))
  `(progn
     (defun ,name ,(mapcar #'car args)
       ,docstring
       (interactive (list ,@(mapcar (lambda (a) `(qss-accept ',(nth 1 a) ,(nth 2 a))) args)))
       ,@body)
     (setf (alist-get ',name qss-commands) (list ,title ',args))
     ',name))

(defun qss-commands-for (object type)
  "Return the actions of `qss-commands' for OBJECT of TYPE: (LABEL . FUNCTION)."
  (let ((result nil))
    (dolist (c qss-commands)
      (pcase-let ((`(,name ,title ,args) c))
        (when (and args (qss-presentation-subtype-p type (nth 1 (car args))))
          (push (cons (if (cdr args) (concat title "...") title)
                      (lambda ()
                        (apply name object
                               (mapcar (lambda (a) (qss-accept (nth 1 a) (nth 2 a))) (cdr args)))))
                result))))
    (sort result (lambda (a b) (string< (car a) (car b))))))

;; A few commands, as examples and for daily use.

(qss-define-command qss-compare-files "Compare with file"
    ((a file "Compare file: ") (b file "with file: "))
  "Show the differences between the files A and B (with diff)."
  (diff (qss-file-name a) (qss-file-name b)))

(qss-define-command qss-copy-file-into "Copy into directory"
    ((file file "Copy file: ") (dir directory "into directory: "))
  "Copy FILE into the directory DIR; ask before overwriting."
  (let* ((from (qss-file-name file))
         (to (expand-file-name (file-name-nondirectory (directory-file-name from))
                               (qss-file-name dir))))
    (if (file-directory-p from)
        (copy-directory from to nil t)
      (copy-file from to 1 t))
    (message "Copied %s to %s" (abbreviate-file-name from) (abbreviate-file-name to))))

(qss-define-command qss-files-larger-than "Files larger than this, here"
    ((size size "Files larger than: "))
  "Show the files in `default-directory' larger than SIZE (bytes, or \"10kb\")."
  (qss-display (qss-> (files) (where (> :size size)) (sort-by size :desc))))

(provide 'qss-present)
;;; qss-present.el ends here
