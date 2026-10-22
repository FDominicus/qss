;;; qss-repl.el --- A REPL for qss: tables right in the buffer  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; M-x qss opens a buffer in the spirit of IELM and Nushell: type a
;; pipe, get the table right below it.
;;
;;   qss> (files "~") (where (> :size 100000)) (sort-by size :desc) (first 5)
;;   files ~/ | where (> :size 100000) | sort-by size desc | first 5   5 rows
;;   name                  type  size  modified          ...
;;   ...
;;   qss> (where (like :name "%.el"))         <- continues the table above
;;
;; Input:
;;   - Several forms, or one form starting with a source (files,
;;     processes, sqlite, tables), are a pipe as in `qss->'.
;;   - A form starting with a word (where, select, sort-by ...)
;;     continues the last table, like $in in Nushell.
;;   - Anything else is ordinary Lisp; wrap it in (progn ...) if it
;;     looks like a word, e.g. Lisp's own `last' or `get'.
;;   - The last table is also in the variable `qss-last', for Lisp.
;;
;; In a table: RET runs the default action of the row (open the file
;; ...), `a' offers its actions, `S' sorts by the column at point.
;; Long tables show the first `qss-repl-max-rows' rows and a [show all]
;; button.  [buffer] opens the table in a buffer of its own, where `g'
;; reads it again.
;;
;; Keys: RET send (or a newline while the input is not complete),
;; M-p / M-n history, TAB completion of words, sources and the column
;; names of the last table, C-c M-o clear the buffer.

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'qss-sql)

(defcustom qss-repl-max-rows 30
  "Maximum number of rows a table shows inside the qss REPL."
  :type 'integer
  :group 'qss)

(defcustom qss-repl-prompt "qss> "
  "Prompt of the qss REPL."
  :type 'string
  :group 'qss)

(defface qss-repl-prompt '((t :inherit minibuffer-prompt))
  "Face of the qss REPL prompt."
  :group 'qss)

(defface qss-repl-title '((t :inherit bold))
  "Face of the title line above a table in the qss REPL."
  :group 'qss)

(defface qss-repl-error '((t :inherit error))
  "Face of error messages in the qss REPL."
  :group 'qss)

(defvar qss-last nil
  "The last table or query shown in the qss REPL.")

(defvar-local qss-repl--input-start nil
  "Marker where the current input begins.")

(defvar-local qss-repl--history nil
  "Inputs of this REPL, newest first.")

(defvar-local qss-repl--history-pos nil
  "Position in `qss-repl--history' while moving through it.
See `qss-repl-previous-input' and `qss-repl-next-input'.")

(defun qss-repl--banner ()
  "Return the text at the top of a new qss REPL."
  (concat
   ";; qss.  Type a pipe and RET, e.g.\n"
   ";;   (files \"~\") (where (> :size 100000)) (sort-by size :desc) (first 5)\n"
   ";;   (sqlite (qss-demo-database) \"Orders\") (where (> :amount 100))\n"
   ";; A line starting with a word, e.g. (where ...), continues the last table.\n"
   ";; RET/a/S on a table row; M-p/M-n history; TAB completes; M-x qss-examples.\n"))

;;; ------------------------------------------------------------------
;;; Mode

(defvar qss-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'qss-repl-send)
    (define-key m (kbd "M-p") #'qss-repl-previous-input)
    (define-key m (kbd "M-n") #'qss-repl-next-input)
    (define-key m (kbd "TAB") #'completion-at-point)
    (define-key m (kbd "C-c M-o") #'qss-repl-clear)
    m)
  "Keymap of `qss-repl-mode'.")

(define-derived-mode qss-repl-mode lisp-data-mode "qss"
  "Major mode of the qss REPL.
\\{qss-repl-mode-map}"
  ;; Output contains vtables with faces of their own; font-lock would
  ;; paint over them (a quote or a semicolon in a file name, say).
  (setq-local font-lock-defaults nil)
  (font-lock-mode -1)
  (setq-local truncate-lines t)
  (setq-local qss--vtable-buffer t)
  (setq qss-repl--input-start (make-marker))
  (add-hook 'completion-at-point-functions #'qss-repl-completion-at-point nil t))

;;;###autoload
(defun qss ()
  "Open the qss REPL: type a pipe, see the table."
  (interactive)
  ;; All sources (nu, sqlite ...), also when this command was autoloaded
  ;; on its own.
  (require 'qss)
  (let ((buf (get-buffer-create "*qss*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'qss-repl-mode)
        (qss-repl-mode)
        (let ((inhibit-read-only t))
          (insert (propertize (qss-repl--banner) 'face 'font-lock-comment-face)))
        (qss-repl--prompt)))
    (pop-to-buffer-same-window buf)
    (goto-char (point-max))
    buf))

;;; ------------------------------------------------------------------
;;; Prompt and output

(defun qss-repl--protect (start end)
  "Make the text from START to END read-only (output and old input)."
  (add-text-properties start end '(read-only t rear-nonsticky (read-only)
                                             front-sticky (read-only))))

(defun qss-repl--prompt ()
  "Insert a fresh prompt at the end of the buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (propertize qss-repl-prompt 'face 'qss-repl-prompt))
    (qss-repl--protect (point-min) (point))
    (set-marker qss-repl--input-start (point))))

(defun qss-repl--button (label action)
  "Insert a button LABEL for ACTION, a function of no arguments."
  (insert-text-button label 'action (lambda (_) (funcall action)) 'follow-link t))

(defun qss-repl--table-part (value)
  "Return (TABLE SHOWN TOTAL) for VALUE, a table or a query.
SHOWN is a table with at most `qss-repl-max-rows' rows; TOTAL is the
number of all rows."
  (let ((max qss-repl-max-rows))
    (if (qss-relation-p value)
        (let* ((rows (qss-sql--run value (1+ max)))
               (total (if (> (length rows) max) (qss-count value) (length rows))))
          (list value
                (qss-sql--table value (take max rows) (qss-relation-title value))
                total))
      (let ((rows (qss-table-rows value)))
        (list value
              (qss-make-table (qss-table-title value) (take max rows)
                              (qss-table-columns value) (qss-table-source value))
              (length rows))))))

(defun qss-repl--insert-table (value)
  "Insert VALUE, a table or a query, as a title line and a vtable."
  (pcase-let ((`(,whole ,shown ,total) (qss-repl--table-part value)))
    ;; Count and button first: a long pipe would push them out of sight.
    (insert (format "%d row%s  " total (if (= total 1) "" "s")))
    (qss-repl--button "[buffer]" (lambda () (qss-display whole)))
    (insert "  " (propertize (qss-table-title shown) 'face 'qss-repl-title) "\n")
    (qss-insert-vtable shown)
    ;; vtable leaves point after its header line, not after the table
    (goto-char (point-max))
    (when (> total (length (qss-table-rows shown)))
      (insert (format "... %d more  " (- total (length (qss-table-rows shown)))))
      (qss-repl--button "[show all]" (lambda () (qss-display whole)))
      (insert "\n"))))

(defun qss-repl--print (value)
  "Insert VALUE as the result of an input."
  (cond ((or (qss-table-p value) (qss-relation-p value))
         (qss-repl--insert-table value))
        ((stringp value) (insert value "\n"))
        (t (let ((print-length 50) (print-level 6))
             (insert (pp-to-string value))))))

;;; ------------------------------------------------------------------
;;; Reading and evaluating

(defun qss-repl--read-forms (input)
  "Return the forms in the string INPUT; signal `end-of-file' if incomplete."
  (car (read-from-string (concat "(" input "\n)"))))

(defun qss-repl--incomplete-p (input)
  "Return non-nil if INPUT is not yet a complete sequence of forms."
  (condition-case nil
      (progn (qss-repl--read-forms input) nil)
    (end-of-file t)
    (error nil)))

(defun qss-repl--word-p (form)
  "Return non-nil if the head of FORM is a word of `qss->'."
  (let ((head (if (consp form) (car form) form)))
    (and (symbolp head) (assq head qss--words))))

(defun qss-repl--source-p (form)
  "Return non-nil if FORM is a source of `qss->', with or without parentheses."
  (let ((head (if (consp form) (car form) form)))
    (and (symbolp head) (assq head qss-sources))))

(defun qss-repl-evaluate (forms)
  "Evaluate FORMS, the forms of one input, and return the value.
See the commentary of qss-repl.el for how the input is understood."
  (cond
   ((null forms) nil)
   ;; continue the last table
   ((and (qss-repl--word-p (car forms))
         (or (qss-table-p qss-last) (qss-relation-p qss-last)))
    (eval `(qss-> qss-last ,@forms) t))
   ;; a pipe
   ((or (cdr forms) (qss-repl--source-p (car forms)))
    (let ((source (if (symbolp (car forms)) (list (car forms)) (car forms))))
      (eval `(qss-> ,source ,@(cdr forms)) t)))
   ;; Lisp
   (t (eval (car forms) t))))

(defun qss-repl-send ()
  "Evaluate the input, or insert a newline while it is incomplete.
On output text, move to the end of the input."
  (interactive)
  (if (< (point) qss-repl--input-start)
      (goto-char (point-max))
    (let ((input (buffer-substring-no-properties qss-repl--input-start (point-max))))
      (if (qss-repl--incomplete-p input)
          (newline-and-indent)
        (unless (string-blank-p input)
          (push (string-trim input) qss-repl--history))
        (setq qss-repl--history-pos nil)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n")
          (condition-case err
              (let ((value (qss-repl-evaluate (qss-repl--read-forms input))))
                (when (or (qss-table-p value) (qss-relation-p value))
                  (setq qss-last value))
                (unless (string-blank-p input)
                  (qss-repl--print value)))
            (error
             (insert (propertize (error-message-string err) 'face 'qss-repl-error) "\n"))))
        (qss-repl--prompt)))))

;;; ------------------------------------------------------------------
;;; History, clearing, completion

(defun qss-repl--replace-input (text)
  "Replace the current input with TEXT."
  (delete-region qss-repl--input-start (point-max))
  (goto-char (point-max))
  (insert text))

(defun qss-repl-previous-input ()
  "Replace the input with the previous one from the history."
  (interactive)
  (when qss-repl--history
    (setq qss-repl--history-pos
          (min (1- (length qss-repl--history)) (1+ (or qss-repl--history-pos -1))))
    (qss-repl--replace-input (nth qss-repl--history-pos qss-repl--history))))

(defun qss-repl-next-input ()
  "Replace the input with the next one from the history."
  (interactive)
  (when qss-repl--history-pos
    (setq qss-repl--history-pos (1- qss-repl--history-pos))
    (qss-repl--replace-input (if (< qss-repl--history-pos 0)
                                 (progn (setq qss-repl--history-pos nil) "")
                               (nth qss-repl--history-pos qss-repl--history)))))

(defun qss-repl-clear ()
  "Delete everything above the current input."
  (interactive)
  (let ((inhibit-read-only t)
        (input (buffer-substring-no-properties qss-repl--input-start (point-max))))
    (erase-buffer)
    (qss-repl--prompt)
    (insert input)))

(defun qss-repl--column-names ()
  "Return the column names of `qss-last', as symbols."
  (cond ((qss-table-p qss-last) (mapcar #'car (qss-table-columns qss-last)))
        ((qss-relation-p qss-last) (mapcar #'car (qss-relation-columns qss-last)))))

(defun qss-repl-completion-at-point ()
  "Complete words, sources and the column names of the last table.
Column names are offered plain (for select, sort-by ...) and as
keywords (for conditions)."
  (when (and qss-repl--input-start (>= (point) qss-repl--input-start))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((columns (qss-repl--column-names)))
          (list (car bounds) (cdr bounds)
                (append (mapcar (lambda (w) (symbol-name (car w))) qss--words)
                        (mapcar (lambda (s) (symbol-name (car s))) qss-sources)
                        (mapcar #'symbol-name columns)
                        (mapcar (lambda (c) (format ":%s" c)) columns)
                        '("qss-demo-database" "qss-last"))
                :exclusive 'no))))))

(provide 'qss-repl)
;;; qss-repl.el ends here
