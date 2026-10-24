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
;; A key for the first one, say:
;;   (keymap-global-set "C-c x" #'qss-xref-uses)

;;; Code:

(require 'qss-table)
(require 'qss-pipe)
(require 'qss-sql)
(require 'qss-menu)
(require 'qss-elisp-xref)
(require 'thingatpt)

(defvar qss--whole)

(defun qss-xref--db ()
  "The database file, or an error if there is none yet."
  (let ((file (qss-elisp-xref-database-file)))
    (unless (file-exists-p file)
      (user-error "No cross reference database %s; make one with M-x qss-elisp-xref-index"
                  (abbreviate-file-name file)))
    file))

(defun qss-xref--source (table)
  "The TABLE of the database, as a qss source."
  (qss-sqlite (qss-xref--db) table))

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
  "Show the query REL under TITLE."
  (setf (qss-relation-title rel) title)
  (qss-display rel))

;;;###autoload
(defun qss-xref-uses (name)
  "Show where NAME appears: line, column and what it is there.
The column name is the definition the use is in (nil: the top level)."
  (interactive (list (qss-xref--read "Uses of")))
  (qss-xref--show
   (qss-> (qss-xref--source "occurrences") (where (= :value name))
          (join (qss-xref--source "definitions") :on (definition-id id) :kind left)
          (join (qss-xref--source "files") :on (file-id id))
          (select library name line col context parent-head path)
          (sort-by library))
   (format "Uses of %s" name)))

;;;###autoload
(defun qss-xref-callers (name)
  "Show the functions that call NAME, after macro expansion.
Each at the line of its definition; a caller nil is the top level of
the file (which includes the bodies of cl-defmethod)."
  (interactive (list (qss-xref--read "Callers of")))
  (qss-xref--show
   (qss-> (qss-xref--source "calls") (where (= :callee name))
          (join (qss-xref--source "definitions") :on ((file-id file-id) (caller name)) :kind left)
          (join (qss-xref--source "files") :on (file-id id))
          (select library caller line path)
          (sort-by caller))
   (format "Callers of %s" name)))

;;;###autoload
(defun qss-xref-callees (name)
  "Show what the function NAME calls, after macro expansion."
  (interactive (list (qss-xref--read "Callees of")))
  (qss-xref--show
   (qss-> (qss-xref--source "calls") (where (= :caller name))
          (join (qss-xref--source "files") :on (file-id id))
          (select callee library path)
          (sort-by callee))
   (format "Callees of %s" name)))

;;;###autoload
(defun qss-xref-definitions (name)
  "Show where NAME is defined: kind, arglist, line and docstring.
Primitives and special forms of C have no file."
  (interactive (list (qss-xref--read "Definitions of")))
  (qss-xref--show
   (qss-> (qss-xref--source "definitions") (where (= :name name))
          (join (qss-xref--source "files") :on (file-id id) :kind left)
          (select name library kind arglist line doc path))
   (format "Definitions of %s" name)))

(defconst qss-xref-text-functions
  '("message" "user-error" "error" "format-message" "read-string" "read-from-minibuffer"
    "completing-read" "y-or-n-p" "yes-or-no-p" "insert" "princ")
  "The functions whose strings are texts for people.")

;;;###autoload
(defun qss-xref-texts (library)
  "Show the texts LIBRARY shows to people (see `qss-xref-text-functions')."
  (interactive (list (read-string (format-prompt "Texts of library"
                                                 (and buffer-file-name
                                                      (file-name-base (file-name-sans-extension buffer-file-name))))
                                  nil nil
                                  (and buffer-file-name
                                       (file-name-base (file-name-sans-extension buffer-file-name))))))
  (qss-xref--show
   (qss-> (qss-xref--source "occurrences")
          (where (and (= :context "string") (in :parent-head qss-xref-text-functions)))
          (join (qss-xref--source "files") :on (file-id id))
          (where (= :library library))
          (select parent-head line col value path))
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

(provide 'qss-elisp-xref-commands)
;;; qss-elisp-xref-commands.el ends here
