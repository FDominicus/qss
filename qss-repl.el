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
;;   - !PROGRAM ARGS runs a program directly, without a shell:
;;       !fossil changes      !git log --oneline -5      !make test
;;     The output appears as text; it is also the last table, with one
;;     row per line in the column `line', so the next input can go on
;;     with (where (like :line "%error%")).  No globs and no pipes in
;;     these lines: for that, (nu "ls *.el | ...").
;;   - cd DIR changes the directory of the REPL (cd .. and plain cd for
;;     home work too); the prompt shows it.  files, nu, run and !
;;     all run there, on the remote host in a TRAMP directory.
;;   - Several forms, or one form starting with a source (files,
;;     processes, sqlite, tables, nu, run), are a pipe as in `qss->'.
;;   - A form starting with a word (where, select, sort-by ...)
;;     continues the last table, like $in in Nushell.
;;   - Anything else is ordinary Lisp; wrap it in (progn ...) if it
;;     looks like a word, e.g. Lisp's own `last' or `get'.
;;   - The last table is also in the variable `qss-last', for Lisp.
;;
;; In a table: RET runs the default action of the row (open the file,
;; enter a directory: cd plus its files), `a' offers its actions, `S'
;; sorts by the column at point.
;; Long tables show the first `qss-repl-max-rows' rows and a [show all]
;; button.  [buffer] opens the table in a buffer of its own, where `g'
;; reads it again.
;;
;; Keys: RET send (or a newline while the input is not complete),
;; M-p / M-n history, TAB completion of words, sources and the column
;; names of the last table, and of file names after ! and cd and inside
;; strings; C-c M-o clear the buffer.

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

(defface qss-repl-directory '((t :inherit font-lock-keyword-face))
  "Face of the directory in the qss REPL prompt."
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
   ";;   !git log --oneline -5       runs a program, no shell\n"
   ";;   cd ~/src                    changes the directory\n"
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
  (setq-local qss-row-action-function #'qss-repl--row-action)
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
    (insert (propertize (abbreviate-file-name default-directory) 'face 'qss-repl-directory)
            " "
            (propertize qss-repl-prompt 'face 'qss-repl-prompt))
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

;;; Shell lines: !program and cd

(defconst qss-repl--cd-regexp "\\`[ \t]*!?cd\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*\\'"
  "A cd line; group 1 is the directory.")

(defun qss-repl--shell-line-p (input)
  "Return non-nil if INPUT is a shell line (!program or cd) rather than Lisp."
  (or (string-match-p "\\`[ \t]*!" input)
      (string-match-p qss-repl--cd-regexp input)))

(defun qss-repl-split-words (line)
  "Split LINE into words as a shell would, but without its other powers.
Whitespace separates words; \"...\" and '...' keep them together (with
backslash escapes inside double quotes); a backslash protects the next
character.  ; | > & * have no special meaning: there is no shell."
  (let ((words nil) (word nil) (in-word nil) (i 0) (n (length line)))
    (while (< i n)
      (let ((c (aref line i)))
        (cond
         ((memq c '(?\s ?\t ?\n))
          (when in-word (push (apply #'string (nreverse word)) words))
          (setq word nil in-word nil))
         ((eq c ?\\)
          (setq in-word t)
          (when (< (1+ i) n) (setq i (1+ i)) (push (aref line i) word)))
         ((eq c ?')
          (setq in-word t)
          (let ((end (or (cl-position ?' line :start (1+ i))
                         (user-error "Missing closing ' in: %s" line))))
            (setq word (append (nreverse (string-to-list (substring line (1+ i) end))) word)
                  i end)))
         ((eq c ?\")
          (setq in-word t i (1+ i))
          (while (and (< i n) (not (eq (aref line i) ?\")))
            (when (and (eq (aref line i) ?\\) (< (1+ i) n))
              (setq i (1+ i)))
            (push (aref line i) word)
            (setq i (1+ i)))
          (unless (< i n) (user-error "Missing closing \" in: %s" line)))
         (t (setq in-word t) (push c word))))
      (setq i (1+ i)))
    (when in-word (push (apply #'string (nreverse word)) words))
    (nreverse words)))

(defun qss-repl--expand-arg (word)
  "Return WORD with a leading ~ expanded, as a shell would."
  (if (string-match-p "\\`~\\(?:/\\|\\'\\)" word)
      (expand-file-name word)
    word))

(defun qss-repl-cd (dir)
  "Change the directory of the REPL to DIR (home if DIR is nil or empty)."
  (let* ((dir (if (or (null dir) (string-empty-p dir)) "~" dir))
         (dir (if (string-match "\\`\"\\(.*\\)\"\\'" dir) (match-string 1 dir) dir))
         (full (file-name-as-directory (expand-file-name dir))))
    (unless (file-directory-p full)
      (user-error "No such directory: %s" full))
    (setq default-directory full)))

(defun qss-repl--run-line (input)
  "Run the shell line INPUT; insert its output and return the value for `qss-last'."
  (if (string-match qss-repl--cd-regexp input)
      (progn (qss-repl-cd (match-string 1 input)) nil)
    (let* ((words (mapcar #'qss-repl--expand-arg
                          (qss-repl-split-words
                           (substring (string-trim-left input) 1)))))
      (unless words (user-error "Nothing to run after !"))
      (let ((table (apply #'qss-run words)))
        (dolist (row (qss-table-rows table))
          (insert (plist-get row :line) "\n"))
        (when (string-match "\\[exit \\(.*\\)\\]\\'" (qss-table-title table))
          (insert (propertize (format "[exit %s]" (match-string 1 (qss-table-title table)))
                              'face 'qss-repl-error)
                  "\n"))
        table))))

(defun qss-repl-send ()
  "Evaluate the input, or insert a newline while it is incomplete.
On output text, move to the end of the input."
  (interactive)
  (if (< (point) qss-repl--input-start)
      (goto-char (point-max))
    (let* ((input (buffer-substring-no-properties qss-repl--input-start (point-max)))
           (shell (qss-repl--shell-line-p input)))
      (if (and (not shell) (qss-repl--incomplete-p input))
          (newline-and-indent)
        (unless (string-blank-p input)
          (push (string-trim input) qss-repl--history))
        (setq qss-repl--history-pos nil)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n")
          (condition-case err
              (if shell
                  (let ((value (qss-repl--run-line input)))
                    (when value (setq qss-last value)))
                (let ((value (qss-repl-evaluate (qss-repl--read-forms input))))
                  (when (or (qss-table-p value) (qss-relation-p value))
                    (setq qss-last value))
                  (unless (string-blank-p input)
                    (qss-repl--print value))))
            (error
             (insert (propertize (error-message-string err) 'face 'qss-repl-error) "\n"))))
        (qss-repl--prompt)))))

(defun qss-repl-submit (input)
  "Put INPUT at the prompt and send it, as if typed."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (delete-region qss-repl--input-start (point-max)))
  (insert input)
  (qss-repl-send))

(defun qss-repl--row-directory (row)
  "Return the directory ROW stands for, or nil.
A directory row from `files', or a row with :type \"dir\" from Nushell."
  (cond ((and (qss-file-p row) (qss-field row 'dir-p)) (qss-file-path row))
        ((and (consp row) (keywordp (car row))
              (equal (plist-get row :type) "dir") (stringp (plist-get row :name)))
         (expand-file-name (plist-get row :name)
                           (or (and (boundp 'qss-nu--dirs) (gethash row qss-nu--dirs))
                               default-directory)))))

(defun qss-repl--row-action (row)
  "Run RET on ROW in the REPL: enter a directory in place, otherwise as usual."
  (let ((dir (qss-repl--row-directory row)))
    (if dir
        (progn (qss-repl-submit (concat "cd " (abbreviate-file-name dir)))
               (qss-repl-submit "(files)"))
      (qss-default-action row))))

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

(defun qss-repl--file-completion ()
  "Return a completion of the file name before point, or nil.
In a shell line (!program, cd) and inside a string."
  (let ((input (buffer-substring-no-properties qss-repl--input-start (point-max))))
    (cond
     ((qss-repl--shell-line-p input)
      (let ((start (save-excursion
                     (skip-chars-backward "^ \t\n" qss-repl--input-start)
                     (when (eq (char-after) ?!) (forward-char))
                     (point))))
        (list start (point) #'completion-file-name-table :exclusive 'no)))
     ((nth 3 (syntax-ppss))
      (list (1+ (nth 8 (syntax-ppss))) (point) #'completion-file-name-table
            :exclusive 'no)))))

(defun qss-repl-completion-at-point ()
  "Complete words, sources and the column names of the last table.
Column names are offered plain (for select, sort-by ...) and as
keywords (for conditions).  After ! and cd and inside a string, file
names are completed."
  (when (and qss-repl--input-start (>= (point) qss-repl--input-start))
   (or
    (qss-repl--file-completion)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((columns (qss-repl--column-names)))
          (list (car bounds) (cdr bounds)
                (append (mapcar (lambda (w) (symbol-name (car w))) qss--words)
                        (mapcar (lambda (s) (symbol-name (car s))) qss-sources)
                        (mapcar #'symbol-name columns)
                        (mapcar (lambda (c) (format ":%s" c)) columns)
                        '("qss-demo-database" "qss-last"))
                :exclusive 'no)))))))

(provide 'qss-repl)
;;; qss-repl.el ends here
