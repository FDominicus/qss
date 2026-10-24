;;; qss-elisp-xref-test.el --- Tests for the Elisp cross references  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A small file whose content is known, indexed into a fresh database;
;; the tests compare the rows with what the file says.

;;; Code:

(require 'ert)
(require 'qss-elisp-xref)

(defconst qss-xtest--source
  ";;; sample.el --- A sample  -*- lexical-binding: t -*-

;;; Commentary:

;; Only for the tests.

;;; Code:

(defvar sample-count 0
  \"How often.\")

(defun sample-greet (name &optional loud)
  \"Greet NAME.
Second line of the doc.\"
  (interactive \"sName: \")
  (let ((text (format \"Hello %s\" name)))
    (when loud (setq text (upcase text)))
    (message \"%s\" text)))

(defmacro sample-twice (&rest body)
  \"Run BODY twice.\"
  `(progn ,@body ,@body))

(cl-defgeneric sample-size (thing) \"Size of THING.\")
(cl-defmethod sample-size ((thing string)) (length thing))
(cl-defmethod sample-size :around ((thing list)) (cl-call-next-method))

(defun sample-local (xs)
  (cl-labels ((walk (x) (if (consp x) (walk (car x)) x)))
    (condition-case err
        (mapcar #'walk xs)
      (wrong-type-argument (user-error \"Bad: %S\" err)))))

(defalias 'sample-alias #'sample-greet)
(cl-defstruct (sample-point (:constructor sample-point-make)) \"A point.\" x y)

(defun sample-chars ()
  (list ?\\\" ?a \"after the char\" '(quoted-a quoted-b) (sample-twice (sample-greet \"you\"))))

(provide 'sample)
;;; sample.el ends here
"
  "The file indexed by the tests.")

(defmacro qss-xtest--with-index (var &rest body)
  "Index the sample file into a fresh database; bind VAR to its connection."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let* ((dir (make-temp-file "qss-xref-" t))
            (lisp (expand-file-name "lisp" dir))
            (qss-elisp-xref-database (expand-file-name "x.sqlite3" dir)))
       (make-directory lisp)
       (with-temp-file (expand-file-name "sample.el" lisp) (insert qss-xtest--source))
       (unwind-protect
           (progn
             (let ((inhibit-message t)) (qss-elisp-xref-run (list lisp)))
             (let ((,var (sqlite-open qss-elisp-xref-database)))
               (unwind-protect (progn ,@body) (sqlite-close ,var))))
         (delete-directory dir t)))))

(defun qss-xtest--rows (db sql &rest params)
  "Return the rows of SQL in DB with PARAMS."
  (sqlite-select db sql (vconcat params)))

(ert-deftest qss-xref-file-and-forms ()
  (qss-xtest--with-index db
    (let ((file (car (qss-xtest--rows db "SELECT library, origin, lexical, forms, commentary, error FROM files"))))
      (should (equal (nth 0 file) "sample"))
      (should (equal (nth 1 file) "user"))
      (should (= 1 (nth 2 file)))
      (should (= 11 (nth 3 file)))
      (should (string-match-p "Only for the tests" (nth 4 file)))
      (should (null (nth 5 file))))
    (should (equal (qss-xtest--rows db "SELECT head, start_line, end_line FROM forms WHERE head = 'defun' ORDER BY 2")
                   '(("defun" 12 18) ("defun" 28 32) ("defun" 37 38))))
    (should (string-prefix-p "(defun sample-greet (name &optional loud)"
                             (caar (qss-xtest--rows db "SELECT text FROM forms WHERE start_line = 12"))))))

(ert-deftest qss-xref-definitions ()
  (qss-xtest--with-index db
    (should (equal (qss-xtest--rows db "SELECT name, kind, line, interactive, arglist FROM definitions
                                        WHERE file_id IS NOT NULL ORDER BY line, name")
                   '(("sample-count" "variable" 9 0 nil)
                     ("sample-greet" "function" 12 1 "(name &optional loud)")
                     ("sample-twice" "macro" 20 0 "(&rest body)")
                     ("sample-size" "generic" 24 0 "(thing)")
                     ("sample-size" "method" 25 0 "((thing string))")
                     ("sample-size" "method" 26 0 "((thing list))")
                     ("sample-local" "function" 28 0 "(xs)")
                     ("walk" "local-function" 29 0 "(x)")
                     ("sample-alias" "alias" 34 0 nil)
                     ("sample-point" "struct" 35 0 nil)
                     ("sample-chars" "function" 37 0 "()"))))
    ;; the whole docstring
    (should (equal (caar (qss-xtest--rows db "SELECT doc FROM definitions WHERE name = 'sample-greet'"))
                   "Greet NAME.\nSecond line of the doc."))
    (should (equal (caar (qss-xtest--rows db "SELECT doc FROM definitions WHERE name = 'sample-point'"))
                   "A point."))
    ;; a local function knows its definition
    (should (equal (qss-xtest--rows db "SELECT p.name FROM definitions d JOIN definitions p ON d.parent_id = p.id
                                        WHERE d.name = 'walk'")
                   '(("sample-local"))))
    (should (equal (caar (qss-xtest--rows db "SELECT specializers FROM definitions WHERE kind = 'method' AND line = 26"))
                   "((thing list))"))))

(ert-deftest qss-xref-contexts ()
  (qss-xtest--with-index db
    (cl-flet ((ctx (value line) (mapcar #'car (qss-xtest--rows db "SELECT context FROM occurrences
                                                                   WHERE value = ? AND line = ? ORDER BY col"
                                                                value line))))
      (should (equal (ctx "sample-greet" 12) '("defining")))
      (should (equal (ctx "name" 12) '("parameter")))
      (should (equal (ctx "text" 16) '("binding")))
      (should (equal (ctx "format" 16) '("head")))
      (should (equal (ctx "name" 16) '("other")))
      (should (equal (ctx "let" 16) '("special-form")))
      (should (equal (ctx "when" 17) '("macro-use")))
      (should (equal (ctx "walk" 29) '("defining" "head")))
      (should (equal (ctx "walk" 31) '("function")))
      (should (equal (ctx "err" 30) '("binding")))
      (should (equal (ctx "wrong-type-argument" 32) '("quoted")))
      (should (equal (ctx "quoted-a" 38) '("quoted")))
      (should (equal (ctx "sample-twice" 38) '("macro-use")))
      (should (equal (ctx "body" 22) '("other" "other")))
      (should (equal (ctx "sample-alias" 34) '("defining"))))
    ;; nil and t are not recorded
    (should (null (qss-xtest--rows db "SELECT * FROM occurrences WHERE value IN ('nil', 't')")))
    ;; parent_head and arg_index
    (should (equal (qss-xtest--rows db "SELECT parent_head, arg_index FROM occurrences WHERE value = 'loud' AND line = 17")
                   '(("when" 1))))
    ;; the definition an occurrence is in
    (should (equal (qss-xtest--rows db "SELECT d.name FROM occurrences o JOIN definitions d ON o.definition_id = d.id
                                        WHERE o.value = 'car'")
                   '(("walk"))))))

(ert-deftest qss-xref-strings ()
  (qss-xtest--with-index db
    (should (equal (qss-xtest--rows db "SELECT value, line, col, parent_head, arg_index FROM occurrences
                                        WHERE context = 'string' ORDER BY line, col")
                   '(("sName: " 15 15 "interactive" 1)
                     ("Hello %s" 16 22 "format" 1)
                     ("%s" 18 13 "message" 1)
                     ("Bad: %S" 32 39 "user-error" 1)
                     ("after the char" 38 15 "list" 3)
                     ("you" 38 81 "sample-greet" 1))))
    ;; docstrings are not among them
    (should (null (qss-xtest--rows db "SELECT * FROM occurrences WHERE value LIKE 'Greet%'")))))

(ert-deftest qss-xref-primitives ()
  (qss-xtest--with-index db
    (should (equal (qss-xtest--rows db "SELECT kind FROM definitions WHERE name = 'car'") '(("primitive"))))
    (should (equal (qss-xtest--rows db "SELECT kind FROM definitions WHERE name = 'let'") '(("special-form"))))
    (should (equal (caar (qss-xtest--rows db "SELECT value FROM meta WHERE key = 'emacs-version'"))
                   emacs-version))))

(ert-deftest qss-xref-incremental ()
  (qss-xtest--with-index db
    (let* ((lisp (file-name-directory (caar (qss-xtest--rows db "SELECT path FROM files"))))
           (count (lambda () (caar (qss-xtest--rows db "SELECT count(*) FROM definitions WHERE file_id IS NOT NULL")))))
      (should (= 11 (funcall count)))
      ;; unchanged: not read again
      (should-not (qss-elisp-xref--index-file db (expand-file-name "sample.el" lisp)))
      ;; changed: read again, nothing twice
      (with-temp-file (expand-file-name "sample.el" lisp)
        (insert qss-xtest--source "\n(defun sample-new () 1)\n"))
      (should (qss-elisp-xref--index-file db (expand-file-name "sample.el" lisp)))
      (should (= 12 (funcall count)))
      ;; a second file, then gone again
      (with-temp-file (expand-file-name "other.el" lisp) (insert "(defun other-f () (sample-greet \"x\"))\n"))
      (let ((inhibit-message t)) (qss-elisp-xref-run (list lisp)))
      (should (= 2 (caar (qss-xtest--rows db "SELECT count(*) FROM files"))))
      (delete-file (expand-file-name "other.el" lisp))
      (let ((inhibit-message t)) (qss-elisp-xref-run (list lisp)))
      (should (= 1 (caar (qss-xtest--rows db "SELECT count(*) FROM files"))))
      (should (null (qss-xtest--rows db "SELECT * FROM occurrences WHERE value = 'other-f'"))))))

(ert-deftest qss-xref-print-arglist ()
  ;; an arglist is not code: (function sequence) stays as it is
  (should (equal (qss-elisp-xref--print '(function sequence)) "(function sequence)"))
  (should (equal (qss-elisp-xref--print '(quote x)) "(quote x)")))

(ert-deftest qss-xref-calls ()
  (qss-xtest--with-index db
    (qss-elisp-xref-calls-run)
    (let ((callees (lambda (caller)
                     (mapcar #'car (qss-xtest--rows db "SELECT callee FROM calls WHERE caller = ?" caller)))))
      ;; after macro expansion: no when, but message and format
      (should (member "message" (funcall callees "sample-greet")))
      (should (member "format" (funcall callees "sample-greet")))
      (should-not (member "when" (funcall callees "sample-greet")))
      ;; sample-twice is a macro: gone, sample-greet stays
      (should (member "sample-greet" (funcall callees "sample-chars")))
      (should-not (member "sample-twice" (funcall callees "sample-chars")))
      (should (equal (qss-xtest--rows db "SELECT error FROM compiled") '((nil)))))))

(ert-deftest qss-xref-commands ()
  (require 'qss-elisp-xref-commands)
  (qss-xtest--with-index db
    (qss-elisp-xref-calls-run)
    (let ((buffers nil))
      (unwind-protect
          (cl-flet ((rows (fn name)
                      (with-current-buffer (funcall fn name)
                        (push (current-buffer) buffers)
                        (qss-> (qss-table-rows qss--table) collect))))
            ;; sample-greet is used in its definition, in the alias and in sample-chars
            (let ((uses (rows #'qss-xref-uses "sample-greet")))
              (should (equal (mapcar (lambda (r) (plist-get r :name)) uses)
                             '("sample-greet" "sample-alias" "sample-chars")))
              (should (equal (mapcar (lambda (r) (plist-get r :context)) uses)
                             '("defining" "function" "head")))
              (should (string-suffix-p "sample.el" (plist-get (car uses) :path))))
            (should (equal (mapcar (lambda (r) (plist-get r :caller)) (rows #'qss-xref-callers "sample-greet"))
                           '("sample-chars")))
            (should (member "message" (mapcar (lambda (r) (plist-get r :callee))
                                              (rows #'qss-xref-callees "sample-greet"))))
            (let ((def (car (rows #'qss-xref-definitions "sample-greet"))))
              (should (equal (plist-get def :arglist) "(name &optional loud)"))
              (should (= 12 (plist-get def :line))))
            ;; the questions in the menu of an answer row
            (with-current-buffer (qss-xref-uses "sample-greet")
              (push (current-buffer) buffers)
              (let ((labels (mapcar #'car (qss-actions (car (qss-table-rows qss--table))))))
                (should (member "Callers of sample-greet" labels))))
            ;; elsewhere plist rows keep their actions only
            (should-not (cdr (with-temp-buffer (qss-actions '(:name "x"))))))
        (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(ert-deftest qss-elisp-xref-database-follows-data-directory ()
  "Without a file of its own the database lies in `qss-data-directory'."
  (let ((user-emacs-directory "/home/u/.emacs.d/"))
    (let ((qss-data-directory nil) (qss-elisp-xref-database nil))
      (should (equal (qss-elisp-xref-database-file) (expand-file-name "/home/u/.emacs.d/elisp-xref.sqlite3"))))
    (let ((qss-data-directory "/local/qss/") (qss-elisp-xref-database nil))
      (should (equal (qss-elisp-xref-database-file) (expand-file-name "/local/qss/elisp-xref.sqlite3"))))
    (let ((qss-data-directory "/local/qss/") (qss-elisp-xref-database "/x/xref.db"))
      (should (equal (qss-elisp-xref-database-file) (expand-file-name "/x/xref.db"))))))

(ert-deftest qss-elisp-xref-database-stands-alone ()
  "In an Emacs with only qss-elisp-xref loaded the default is user-emacs-directory."
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (here (file-name-directory (locate-library "qss-elisp-xref"))))
    (with-temp-buffer
      (should (zerop (call-process emacs nil t nil "-Q" "--batch" "-L" here
                                   "-l" "qss-elisp-xref"
                                   "--eval" "(setq user-emacs-directory \"/home/u/.emacs.d/\")"
                                   "--eval" "(princ (qss-elisp-xref-database-file))")))
      (should (string-suffix-p "/home/u/.emacs.d/elisp-xref.sqlite3" (buffer-string))))))

(provide 'qss-elisp-xref-test)
;;; qss-elisp-xref-test.el ends here
