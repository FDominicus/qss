;;; qss-elisp-xref-commands.el --- Commands on the Elisp cross references  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Questions to the database of `qss-elisp-xref', as commands.  Each
;; asks for a name; the default is the name at point: in a qss table
;; the value under point (or the name of the row), elsewhere the
;; symbol under point.  The answer is a qss table; RET on a row opens
;; the file at the place.
;;
;;   M-x qss-xref-uses         where a name appears (calls, #', quoted ...)
;;   M-x qss-xref-callers      who calls a function (from the compiler)
;;   M-x qss-xref-callees      what a function calls
;;   M-x qss-xref-definitions  where a name is defined, with its docstring
;;   M-x qss-xref-texts        the texts a library shows to people
;;
;; In a table of these answers the menu (a, mouse-3) has the same
;; questions for the row: from a use to the callers, and so on.
;;
;; The commands are also in a keymap, for a prefix of one's own:
;;   (keymap-global-set "C-c x" qss-xref-command-map)
;;
;; And the database is a backend of xref, Emacs' own cross references:
;; with `qss-xref-mode' on, M-. jumps to a definition, M-? lists the
;; uses, C-M-. searches the names, M-, goes back -- for all the indexed
;; code, loaded or not.
;;   (add-hook 'emacs-lisp-mode-hook #'qss-xref-mode)
;;
;; In the code of qss a column is written as a keyword (:name), a
;; plain symbol is Lisp (the variable name).

;;; Code:

(require 'qss-table)
(require 'qss-pipe)
(require 'qss-sql)
(require 'qss-menu)
(require 'qss-elisp-xref)
(require 'thingatpt)
(require 'xref)

(defvar qss--whole)

(defun qss-xref--db ()
  "The database file, or an error if there is none yet."
  (let ((file (qss-elisp-xref-database-file)))
    (unless (file-exists-p file)
      (user-error "No cross reference database %s; make one with M-x qss-elisp-xref-index"
                  (abbreviate-file-name file)))
    file))

(defun qss-xref-table (table)
  "The TABLE of the cross reference database, as a qss query.
In a pipe it is the source xref: (qss-> (xref \"files\") ...)."
  (qss-sqlite (qss-xref--db) table))

;; At compile time too: the commands below use the source in `qss->'.
(eval-and-compile (setf (alist-get 'xref qss-sources) 'qss-xref-table))

(defun qss-xref--row-name (row)
  "The name ROW (a row of an answer) is about, or nil."
  (and (consp row) (keywordp (car row))
       (cl-some (lambda (key) (let ((v (plist-get row key))) (and (stringp v) (not (string-empty-p v)) v)))
                '(:name :caller :callee :value))))

(defun qss-xref--name-at-point ()
  "The name at point: in a qss table the value or row there, else the symbol."
  (or (and (derived-mode-p 'qss-table-mode)
           (let* ((row (ignore-errors (vtable-current-object)))
                  (col (ignore-errors (qss--column-at-point)))
                  (value (and row col (memq col '(name caller callee value))
                              (qss-field row col))))
             (if (and (stringp value) (not (string-empty-p value)))
                 value
               (qss-xref--row-name row))))
      (thing-at-point 'symbol t)))

(defun qss-xref--read (prompt)
  "Read a name with PROMPT, the name at point as default."
  (let* ((default (qss-xref--name-at-point))
         (name (read-string (format-prompt prompt default) nil nil default)))
    (when (string-empty-p name) (user-error "No name"))
    name))

(defun qss-xref--show (rel title)
  "Show the query REL under TITLE, with `qss-xref-mode' in its buffer."
  (setf (qss-relation-title rel) title)
  (let ((buffer (qss-display rel)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (qss-xref-mode 1)))
    buffer))

;;; ------------------------------------------------------------------
;;; The questions

;;;###autoload
(defun qss-xref-uses (name)
  "Show where NAME appears: library, line, column and what it is there.
The column :name is the definition the use stands in (nil: the top level)."
  (interactive (list (qss-xref--read "Uses of")))
  (qss-xref--show
   (qss-> (xref "occurrences") (where (= :value name))
          (join (xref "definitions") :on (:definition-id :id) :kind left)
          (join (xref "files") :on (:file-id :id))
          (select :library :name :line :col :context :parent-head :path)
          (sort-by :library))
   (format "Uses of %s" name)))

;;;###autoload
(defun qss-xref-callers (name)
  "Show the functions that call NAME, after macro expansion.
Each at the line of its definition; a caller nil is the top level of
the file (which includes the bodies of cl-defmethod)."
  (interactive (list (qss-xref--read "Callers of")))
  (qss-xref--show
   (qss-> (xref "calls") (where (= :callee name))
          (join (xref "definitions")
                :on ((:file-id :file-id) (:caller :name)) :kind left)
          (join (xref "files") :on (:file-id :id))
          (select :library :caller :line :path)
          (sort-by :caller))
   (format "Callers of %s" name)))

;;;###autoload
(defun qss-xref-callees (name)
  "Show what the function NAME calls, after macro expansion."
  (interactive (list (qss-xref--read "Callees of")))
  (qss-xref--show
   (qss-> (xref "calls") (where (= :caller name))
          (join (xref "files") :on (:file-id :id))
          (select :callee :library :path)
          (sort-by :callee))
   (format "Callees of %s" name)))

;;;###autoload
(defun qss-xref-definitions (name)
  "Show where NAME is defined: kind, arglist, line and docstring.
Primitives and special forms of C have no file."
  (interactive (list (qss-xref--read "Definitions of")))
  (qss-xref--show
   (qss-> (xref "definitions") (where (= :name name))
          (join (xref "files") :on (:file-id :id) :kind left)
          (select :name :library :kind :arglist :line :doc :path))
   (format "Definitions of %s" name)))

(defconst qss-xref-text-functions
  '("message" "user-error" "error" "format-message" "read-string" "read-from-minibuffer"
    "completing-read" "y-or-n-p" "yes-or-no-p" "insert" "princ")
  "The functions whose strings are texts for people.")

(defun qss-xref--library-at-point ()
  "The library of the current buffer's file, or nil."
  (and buffer-file-name
       (file-name-base (file-name-sans-extension buffer-file-name))))

;;;###autoload
(defun qss-xref-texts (library)
  "Show the texts LIBRARY shows to people (see `qss-xref-text-functions')."
  (interactive (let ((default (qss-xref--library-at-point)))
                 (list (read-string (format-prompt "Texts of library" default)
                                    nil nil default))))
  (qss-xref--show
   (qss-> (xref "occurrences")
          (where (and (= :context "string") (in :parent-head qss-xref-text-functions)))
          (join (xref "files") :on (:file-id :id))
          (where (= :library library))
          (select :parent-head :line :col :value :path))
   (format "Texts of %s" library)))

;;; The same questions in the menu of an answer

(defun qss-xref--answer-row-p ()
  "Non-nil in a table read from the cross reference database."
  (and (boundp 'qss--whole) (qss-relation-p qss--whole)
       (let ((db (qss-elisp-xref-database-file)))
         (and (file-exists-p db)
              (file-equal-p (qss-relation-file qss--whole) db)))))

(cl-defmethod qss-actions ((row cons))
  "In an answer of the cross references: the questions for ROW's name too."
  (let ((name (and (qss-xref--answer-row-p) (qss-xref--row-name row))))
    (append (cl-call-next-method)
            (and name
                 (list (cons (format "Uses of %s" name) (lambda (_) (qss-xref-uses name)))
                       (cons (format "Callers of %s" name) (lambda (_) (qss-xref-callers name)))
                       (cons (format "Callees of %s" name) (lambda (_) (qss-xref-callees name)))
                       (cons (format "Definitions of %s" name) (lambda (_) (qss-xref-definitions name))))))))

;;; ------------------------------------------------------------------
;;; Keys

;;;###autoload (autoload 'qss-xref-command-map "qss-elisp-xref-commands" nil t 'keymap)
(defvar-keymap qss-xref-command-map
  :doc "The questions to the cross references, for a prefix key of one's own.
For instance: (keymap-global-set \"C-c x\" qss-xref-command-map)."
  "u" #'qss-xref-uses
  "c" #'qss-xref-callers
  "e" #'qss-xref-callees
  "d" #'qss-xref-definitions
  "t" #'qss-xref-texts
  "i" #'qss-elisp-xref-index
  "m" #'qss-xref-mode)

;;; ------------------------------------------------------------------
;;; A backend of xref: M-. M-? C-M-. (and M-, back)

(defcustom qss-xref-fallback t
  "Non-nil means: ask Emacs' own Elisp backend when the database knows nothing.
That covers code not indexed yet, and the C source of primitives."
  :type 'boolean
  :group 'qss-elisp-xref)

(defun qss-xref-backend ()
  "The xref backend of the cross reference database, when the database exists."
  (and (file-exists-p (qss-elisp-xref-database-file)) 'qss-xref))

;;;###autoload
(define-minor-mode qss-xref-mode
  "Let M-. M-? and C-M-. ask the cross reference database.
M-. jumps to a definition, M-? lists every use, C-M-. finds
definitions by parts of their name, M-, goes back.  All indexed code
is found, whether loaded or not.  Where the database knows nothing,
Emacs' own Elisp backend answers (see `qss-xref-fallback')."
  :lighter " qx"
  (if qss-xref-mode
      (add-hook 'xref-backend-functions #'qss-xref-backend -50 t)
    (remove-hook 'xref-backend-functions #'qss-xref-backend t)))

(defmacro qss-xref--with-db (var &rest body)
  "Run BODY with VAR an open connection to the database."
  (declare (indent 1))
  `(let ((,var (sqlite-open (qss-xref--db))))
     (unwind-protect (progn ,@body) (sqlite-close ,var))))

(defun qss-xref--fallback (fn identifier)
  "Ask the Elisp backend with FN for IDENTIFIER, if it is there and wanted."
  (and qss-xref-fallback
       (fboundp 'elisp--xref-backend)
       (ignore-errors (funcall fn 'elisp identifier))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql qss-xref)))
  (qss-xref--name-at-point))

(defvar qss-xref--names nil
  "Cache (FILE MTIME . NAMES) of the defined names, for completion.")

(defun qss-xref--defined-names ()
  "All names the database defines, cached until the file changes."
  (let* ((file (qss-xref--db))
         (mtime (file-attribute-modification-time (file-attributes file))))
    (unless (and qss-xref--names
                 (equal (car qss-xref--names) file)
                 (time-equal-p (cadr qss-xref--names) mtime))
      (setq qss-xref--names
            (cons file (cons mtime
                             (qss-xref--with-db db
                               (mapcar #'car (sqlite-select db "SELECT DISTINCT name FROM definitions")))))))
    (cddr qss-xref--names)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql qss-xref)))
  (qss-xref--defined-names))

(defun qss-xref--definition-items (db sql params)
  "The xref items of the definitions SQL selects with PARAMS in DB.
SQL yields name, head, arglist, line and path."
  (delq nil
        (mapcar (pcase-lambda (`(,name ,head ,arglist ,line ,path))
                  (and path
                       (xref-make (format "(%s %s%s)" head name
                                          (if (stringp arglist) (concat " " arglist) ""))
                                  (xref-make-file-location path (or line 1) 0))))
                (sqlite-select db sql params))))

(defconst qss-xref--definitions-sql
  "SELECT d.name, CASE WHEN d.parent_id IS NULL THEN m.head ELSE d.kind END,
          d.arglist, d.line, f.path
     FROM definitions d LEFT JOIN files f ON f.id = d.file_id
                        LEFT JOIN forms m ON m.id = d.form_id
    WHERE %s ORDER BY f.path, d.line"
  "The definitions with their files; %s is the condition.
The second column is the defining form (defun, cl-defmethod ...), or the
kind for a local function.")

(cl-defmethod xref-backend-definitions ((_backend (eql qss-xref)) identifier)
  (or (qss-xref--with-db db
        (qss-xref--definition-items
         db (format qss-xref--definitions-sql "d.name = ?") (vector identifier)))
      (qss-xref--fallback #'xref-backend-definitions identifier)))

(cl-defmethod xref-backend-references ((_backend (eql qss-xref)) identifier)
  (or (qss-xref--with-db db
        (let ((forms (make-hash-table)))
          (mapcar (pcase-lambda (`(,path ,line ,col ,form-id ,start ,text))
                    ;; the text of a form once, even with many uses in it
                    (let ((lines (or (gethash form-id forms)
                                     (puthash form-id (split-string (or text "") "\n") forms))))
                      (xref-make-match (string-trim (or (nth (- line start) lines) ""))
                                       (xref-make-file-location path line col)
                                       (length identifier))))
                  (sqlite-select db "SELECT f.path, o.line, o.col, o.form_id, m.start_line, m.text
                                       FROM occurrences o
                                       JOIN files f ON f.id = o.file_id
                                       LEFT JOIN forms m ON m.id = o.form_id
                                      WHERE o.value = ? AND o.context <> 'string'
                                      ORDER BY f.path, o.line, o.col"
                                 (vector identifier)))))
      (qss-xref--fallback #'xref-backend-references identifier)))

(defun qss-xref--like-patterns (pattern)
  "The LIKE patterns for the words of PATTERN (all must match)."
  (mapcar (lambda (w) (concat "%" (replace-regexp-in-string "[%_\\]" "\\\\\\&" w) "%"))
          (split-string pattern "[ \t]+" t)))

(cl-defmethod xref-backend-apropos ((_backend (eql qss-xref)) pattern)
  (let ((likes (qss-xref--like-patterns pattern)))
    (when likes
      (qss-xref--with-db db
        (qss-xref--definition-items
         db (format qss-xref--definitions-sql
                    (mapconcat (lambda (_) "d.name LIKE ? ESCAPE '\\'") likes " AND "))
         (vconcat likes))))))

(provide 'qss-elisp-xref-commands)
;;; qss-elisp-xref-commands.el ends here
