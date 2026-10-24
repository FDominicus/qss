;;; qss-menu.el --- Actions on the row, the value and the table at point  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; On a table row, \\`a' (or the right mouse button) offers three kinds
;; of actions:
;;
;;   - the actions of the row object: open the file, kill the process ...
;;     (`qss-actions');
;;   - words with the value at point:  (where (= :status "paid")),
;;     (where (is-null :city)), (sort-by amount :desc), (group-by user),
;;     (select name), (reject mode), copy the value;
;;   - commands with typed arguments (`qss-define-command') whose first
;;     argument fits the row or the value: "Compare with file...", the
;;     other arguments are accepted, e.g. by a click on another row;
;;   - the whole table: copy it as text, Org, Markdown, CSV or TSV.
;;
;; A word continues the whole table, not only the rows shown: on SQLite
;; the query, so (where ...) becomes SQL again.  In the qss REPL the
;; word is submitted as input, so you see what happened and find it in
;; the history; elsewhere the result opens in a buffer of its own.
;;
;; \\`w' copies the whole table in `qss-copy-format' (with a prefix
;; argument it asks for the format).

;;; Code:

(require 'cl-lib)
(require 'qss-present)

(declare-function qss-edit-cell "qss-edit" (&optional make-null))
(declare-function qss-edit-toggle-delete "qss-edit" ())

(defcustom qss-copy-format 'text
  "Format in which \\`w' copies a table; see `qss-to'."
  :type '(choice (const text) (const org) (const md) (const csv) (const tsv))
  :group 'qss)

(defcustom qss-menu-label-width 70
  "Maximum width of a label in the actions at point."
  :type 'integer
  :group 'qss)

(defvar-local qss-continue-function nil
  "Function that continues a table with a word, in this buffer.
It is called with the table (or query) and a stage such as
\(where (= :name \"x\")).  If nil, the result is shown with
`qss-display'.  The qss REPL submits the stage as input instead.")

;;; ------------------------------------------------------------------
;;; What is at point

(defun qss--whole-at-point ()
  "Return the whole table or query at point; signal an error if none."
  (or (qss-table-at-point)
      qss--whole
      qss--table
      (user-error "No table here")))

(defun qss--column-at-point ()
  "Return the name (a symbol) of the column at point, or nil."
  (let ((vt (vtable-current-table))
        (index (vtable-current-column)))
    (and vt index (intern (vtable-column vt index)))))

(defun qss--spec-at (whole name)
  "Return the column spec NAME of WHOLE, a table or a query."
  (if (qss-table-p whole)
      (qss--spec-of whole name)
    (qss--column-spec name)))

(defun qss--literal (value)
  "Return (FORM) for VALUE as it is written in a condition, or nil.
Strings and numbers stand for themselves, symbols are quoted.  Other
values (lists, objects) cannot be written in a condition."
  (cond ((or (stringp value) (numberp value)) (list value))
        ((and value (symbolp value) (not (keywordp value))) (list `',value))))

(defun qss--day (time &optional days)
  "Return the day of TIME, DAYS later, as text like \"2026-10-22\"."
  (let ((d (decode-time time)))
    (setf (decoded-time-day d) (+ (decoded-time-day d) (or days 0))
          (decoded-time-hour d) 12)       ; away from a change of daylight saving
    (format-time-string "%Y-%m-%d" (encode-time d))))

(defun qss--value-stages (name value spec)
  "Return the words for VALUE in column NAME (with SPEC): a list of stages.
Conditions on the value first, then the words on the column."
  (let ((col (intern (format ":%s" name)))
        (timep (or (qss--time-p value)
                   (and (numberp value) (eq (plist-get (cdr spec) :type) 'time))))
        (literal (qss--literal value)))
    (append
     (cond
      ((null value)
       `((where (is-null ,col)) (where (not-null ,col))))
      (timep
       (let ((day (qss--day value)) (next (qss--day value 1)))
         `((where (and (>= ,col ,day) (< ,col ,next)))
           (where (>= ,col ,day))
           (where (< ,col ,day)))))
      (literal
       (let ((v (car literal)))
         (append `((where (= ,col ,v)) (where (/= ,col ,v)))
                 (and (numberp value) `((where (> ,col ,v)) (where (< ,col ,v))))))))
     `((sort-by ,name) (sort-by ,name :desc) (group-by ,name)
       (select ,name) (reject ,name)))))

(defun qss--label (stage)
  "Return the label of STAGE in the actions."
  (truncate-string-to-width (prin1-to-string stage) qss-menu-label-width nil nil "..."))

(defun qss-actions-at-point ()
  "Return the actions at point as a list of groups.
Each group is a list of (LABEL . FUNCTION); FUNCTION takes no
arguments.  The groups: the row object, the value at point, the
commands for the row or the value (`qss-commands-for'), the table."
  (let* ((whole (qss--whole-at-point))
         (row (vtable-current-object))
         (name (and row (qss--column-at-point)))
         (spec (and name (qss--spec-at whole name)))
         (value (and name (qss-field row name))))
    (delq nil
          (list
           (and row
                (mapcar (lambda (action)
                          (cons (car action) (lambda () (funcall (cdr action) row))))
                        (qss-actions row)))
           (and name
                (append
                 (mapcar (lambda (stage)
                           (cons (qss--label stage) (lambda () (qss-continue whole stage))))
                         (qss--value-stages name value spec))
                 (list (cons (format "Copy value of %s" name)
                             (lambda ()
                               (let ((text (qss--plain-value value spec)))
                                 (kill-new text)
                                 (message "Copied: %s" text)))))))
           ;; changing a stored row, where that is possible (qss-edit.el)
           (and row name (derived-mode-p 'qss-table-mode) (qss-relation-p whole)
                (qss-sql--direct-p whole) (qss-sql-rowid row)
                (list (cons (format "Change %s..." name) (lambda () (qss-edit-cell)))
                      (cons (format "Make %s empty (NULL)" name) (lambda () (qss-edit-cell t)))
                      (cons "Mark row for deletion" (lambda () (qss-edit-toggle-delete)))))
           (let ((row-type (and row (qss-presentation-type row)))
                 (cell-type (and name (qss--cell-type spec value))))
             (append (and row-type (not (eq row-type t)) (qss-commands-for row row-type))
                     (and cell-type (not (eq cell-type t)) (qss-commands-for value cell-type))))
           (mapcar (lambda (format)
                     (cons (format "Copy table as %s" format)
                           (lambda () (qss-copy-table-as whole format))))
                   qss-text-formats)))))

;;; ------------------------------------------------------------------
;;; Running them

(defun qss-continue (table stage)
  "Continue TABLE (or a query) with STAGE, a word such as (sort-by name).
See `qss-continue-function'."
  (if qss-continue-function
      (funcall qss-continue-function table stage)
    (qss-display (eval `(qss-> qss--input ,stage) `((qss--input . ,table))))))

(defun qss--ordered-completion (labels)
  "Return a completion table of LABELS that keeps their order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action labels string pred))))

(defun qss-action-at-point ()
  "Choose one of the actions at point and run it.
The actions of the row, words with the value at point and copying the
table; see `qss-actions-at-point'."
  (interactive)
  (let* ((actions (apply #'append (qss-actions-at-point)))
         (choice (completing-read "Action: " (qss--ordered-completion (mapcar #'car actions))
                                  nil t)))
    (funcall (cdr (assoc choice actions)))))

(defun qss-context-menu (event)
  "Pop up the actions at the mouse position of EVENT."
  (interactive "e")
  (mouse-set-point event)
  (let ((map (make-sparse-keymap "qss"))
        (items nil)
        (n 0))
    (dolist (group (qss-actions-at-point))
      (when items (push 'separator items))
      (dolist (action group) (push action items)))
    ;; `define-key' puts each item in front, so the last one goes first.
    (dolist (item items)
      (define-key map (vector (intern (format "qss-item-%d" (cl-incf n))))
        (if (eq item 'separator)
            '(menu-item "--")
          (let ((fn (cdr item)))
            `(menu-item ,(car item) ,(lambda () (interactive) (funcall fn)))))))
    (popup-menu map event)))

(defun qss-copy-table-as (table format)
  "Copy TABLE (or a query) as text in FORMAT to the kill ring."
  (let ((text (qss-to table format)))
    (kill-new text)
    (message "Copied %d lines as %s" (cl-count ?\n text) format)))

(defun qss-copy-table (&optional ask)
  "Copy the whole table at point as text, in `qss-copy-format'.
With a prefix argument ASK, ask for the format.  A query is copied with
all its rows, not only those shown."
  (interactive "P")
  (qss-copy-table-as (qss--whole-at-point)
                     (if ask
                         (intern (completing-read "Format: " (mapcar #'symbol-name qss-text-formats)
                                                  nil t nil nil (symbol-name qss-copy-format)))
                       qss-copy-format)))

(provide 'qss-menu)
;;; qss-menu.el ends here
