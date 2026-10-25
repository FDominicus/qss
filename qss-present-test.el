;;; qss-present-test.el --- Tests for presentations and accept  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-present-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defmacro qss-prtest--with-db (&rest body)
  "Run BODY with a fresh demo database in a temporary directory."
  (declare (indent 0))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let ((temporary-file-directory (make-temp-file "qss-present-" t)))
       (unwind-protect (progn ,@body)
         (delete-directory temporary-file-directory t)))))

(defmacro qss-prtest--with-dir (var &rest body)
  "Bind VAR to a directory with two files and a subdirectory, then run BODY."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "qss-present-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "old.txt" ,var) (insert "one\ntwo\n"))
           (with-temp-file (expand-file-name "new.txt" ,var) (insert "one\nTWO\n"))
           (make-directory (expand-file-name "target" ,var))
           ,@body)
       (delete-directory ,var t))))

(defmacro qss-prtest--in-repl (&rest body)
  "Run BODY in a fresh qss REPL shown in the selected window."
  (declare (indent 0))
  `(let ((qss-last nil))
     (when (get-buffer "*qss*") (kill-buffer "*qss*"))
     (save-window-excursion
       (with-current-buffer (qss)
         (unwind-protect (progn ,@body)
           (kill-buffer "*qss*"))))))

(defun qss-prtest--send (input)
  "Type INPUT at the prompt and press RET."
  (goto-char (point-max))
  (insert input)
  (qss-repl-send))

(defun qss-prtest--click-on (text)
  "Return a fake `completing-read' that clicks the offered TEXT and returns \"\"."
  (lambda (&rest _)
    (let ((o (cl-find-if (lambda (o) (equal (buffer-substring-no-properties
                                             (overlay-start o) (overlay-end o))
                                            text))
                         qss--accept-overlays)))
      (should o)
      (qss--accept-choose o)
      "")))

;;; ------------------------------------------------------------------
;;; Types

(ert-deftest qss-present-type-hierarchy ()
  (should (qss-presentation-subtype-p 'directory 'file))
  (should (qss-presentation-subtype-p 'directory 'object))
  (should (qss-presentation-subtype-p 'size 'number))
  (should (qss-presentation-subtype-p 'string t))
  (should-not (qss-presentation-subtype-p 'file 'directory))
  (should-not (qss-presentation-subtype-p 'size 'string))
  (should-not (qss-presentation-subtype-p 'row 'value)))

(ert-deftest qss-present-parse ()
  (should (= 10000 (qss-presentation-parse 'size "10kb")))
  (should (= 512 (qss-presentation-parse 'size "512")))
  (should (= 10800000000000 (qss-presentation-parse 'duration "3hr")))
  (should (= 42 (qss-presentation-parse 'integer "42")))
  (should-not (qss-presentation-parse 'integer "4.2"))
  (should (= 4.2 (qss-presentation-parse 'number "4.2")))
  (should (equal "abc" (qss-presentation-parse 'string "abc")))
  (should (equal 42 (qss-presentation-parse 'value "42")))
  (should (qss--time-p (qss-presentation-parse 'time "2026-10-22")))
  (should-not (qss-presentation-parse 'time "yesterday"))
  (qss-prtest--with-dir dir
    (should (equal (qss-presentation-parse 'file (concat dir "old.txt")) (concat dir "old.txt")))
    (should-not (qss-presentation-parse 'directory (concat dir "old.txt")))
    (should (equal (qss-presentation-parse 'directory (concat dir "target")) (concat dir "target/")))
    (should-not (qss-presentation-parse 'file (concat dir "missing")))))

(ert-deftest qss-present-types-of-objects ()
  (qss-prtest--with-dir dir
    (let ((rows (qss-table-rows (qss-files dir))))
      (should (equal (sort (mapcar #'qss-presentation-type rows) #'string<)
                     '(directory file file)))))
  (should (eq (qss-presentation-type 3) 'integer))
  (should (eq (qss-presentation-type 2.5) 'number))
  (should (eq (qss-presentation-type "x") 'string))
  (should (eq (qss-presentation-type (current-time)) 'time))
  (should (eq (qss-presentation-type '(:a 1)) 'row))
  (should (eq (qss-presentation-type (list :path temporary-file-directory)) 'directory))
  (should (eq (qss-presentation-type (car (qss-table-rows (qss-processes)))) 'process)))

;;; ------------------------------------------------------------------
;;; Finding presentations

(ert-deftest qss-present-cells-by-type ()
  "Cells are found by the type of their value; rows by the type of the object."
  (qss-prtest--with-db
    (qss-prtest--in-repl
      (qss-prtest--send "(sqlite (qss-demo-database) \"Orders\")")
      (cl-flet ((found (type) (qss--presentations-in (point-min) (point-max) type)))
        ;; order-no and customer-no of six rows
        (should (= 12 (length (found 'integer))))
        ;; plus the amounts
        (should (= 18 (length (found 'number))))
        ;; dates and status texts (two status are NULL)
        (should (= 10 (length (found 'string))))
        (should (= 28 (length (found 'value))))
        (should (null (found 'file)))
        ;; only the text of a cell, not its padding
        (dolist (p (found 'integer))
          (should (string-match-p "\\`[0-9]+\\'" (buffer-substring-no-properties (nth 0 p) (nth 1 p)))))))))

(ert-deftest qss-present-file-rows ()
  (qss-prtest--with-dir dir
    (save-window-excursion
      (let ((buf (qss-show (qss-files dir))))
        (unwind-protect
            (with-current-buffer buf
              (should (= 3 (length (qss--presentations-in (point-min) (point-max) 'file))))
              (let ((dirs (qss--presentations-in (point-min) (point-max) 'directory)))
                (should (= 1 (length dirs)))
                (should (string-suffix-p "target" (qss-file-path (nth 2 (car dirs))))))
              ;; the sizes of the files are cells of type size
              (should (= 3 (length (qss--presentations-in (point-min) (point-max) 'size)))))
          (kill-buffer buf))))))

(ert-deftest qss-present-explicit ()
  (with-temp-buffer
    (insert "Total: ")
    (qss-present 6110 'size "6.0k")
    (insert " and ")
    (qss-present "plain")
    (let ((sizes (qss--presentations-in (point-min) (point-max) 'size)))
      (should (equal (mapcar (lambda (p) (nth 2 p)) sizes) '(6110)))
      (should (equal (buffer-substring (nth 0 (car sizes)) (nth 1 (car sizes))) "6.0k")))
    (should (= 2 (length (qss--presentations-in (point-min) (point-max) 'value))))))

;;; ------------------------------------------------------------------
;;; Accepting

(ert-deftest qss-present-accept-by-click ()
  "A click returns the object; the highlights are gone afterwards."
  (qss-prtest--with-dir dir
    (save-window-excursion
      (let ((buf (qss-show (qss-files dir))))
        (unwind-protect
            (with-current-buffer buf
              (let ((seen-prompt nil))
                (cl-letf (((symbol-function 'completing-read)
                           (let ((click (qss-prtest--click-on
                                         (buffer-substring-no-properties
                                          (save-excursion (goto-char (point-min))
                                                          (search-forward "target")
                                                          (line-beginning-position))
                                          (save-excursion (goto-char (point-min))
                                                          (search-forward "target")
                                                          (line-end-position))))))
                             (lambda (prompt &rest args)
                               (setq seen-prompt prompt)
                               (apply click prompt args)))))
                  (let ((chosen (qss-accept 'directory "Into: ")))
                    (should (qss-file-p chosen))
                    (should (string-suffix-p "target" (qss-file-path chosen)))))
                (should (string-match-p "\\`Into: (1 shown" seen-prompt)))
              (should (null qss--accept-overlays))
              (should-not (cl-some (lambda (o) (overlay-get o 'qss-object))
                                   (overlays-in (point-min) (point-max)))))
          (kill-buffer buf))))))

(ert-deftest qss-present-accept-by-typing ()
  (qss-prtest--with-dir dir
    (save-window-excursion
      (let ((buf (qss-show (qss-files dir))))
        (unwind-protect
            (with-current-buffer buf
              ;; a label of a shown one
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_p table &rest _)
                           (should (member (abbreviate-file-name (concat dir "new.txt"))
                                           (all-completions "" table)))
                           (abbreviate-file-name (concat dir "new.txt")))))
                (should (qss-file-p (qss-accept 'file))))
              ;; text parsed for the type
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "10kb")))
                (should (= 10000 (qss-accept 'size))))
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "no such file")))
                (should-error (qss-accept 'file) :type 'user-error))
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
                (should-error (qss-accept 'file) :type 'user-error))
              (should (null qss--accept-overlays)))
          (kill-buffer buf))))))

(ert-deftest qss-present-repl-insert-value ()
  "C-c C-v: a click on a cell puts its value into the input."
  (qss-prtest--with-db
    (qss-prtest--in-repl
      (qss-prtest--send "(sqlite (qss-demo-database) \"Orders\")")
      (should (eq (key-binding (kbd "C-c C-v")) #'qss-repl-insert-value))
      (goto-char (point-max))
      (insert "(where (= :order-date ")
      (cl-letf (((symbol-function 'completing-read) (qss-prtest--click-on "2026-10-02")))
        (qss-repl-insert-value))
      (insert "))")
      (should (string-suffix-p "qss> (where (= :order-date \"2026-10-02\"))" (buffer-string)))
      (qss-repl-send)
      (should (equal (qss-> qss-last (get order-no) collect) '(4714)))
      ;; a number goes in as a number
      (goto-char (point-max))
      (insert "(where (> :amount ")
      (cl-letf (((symbol-function 'completing-read) (qss-prtest--click-on "45.0")))
        (qss-repl-insert-value))
      (should (string-suffix-p "(where (> :amount 45.0" (buffer-string))))))

(ert-deftest qss-present-repl-results-are-presentations ()
  (qss-prtest--in-repl
    (qss-prtest--send "(+ 40 2)")
    (qss-prtest--send "(concat \"a\" \"b\")")
    (let ((values (mapcar (lambda (p) (nth 2 p))
                          (qss--presentations-in (point-min) (point-max) 'value))))
      (should (member 42 values))
      (should (member "ab" values)))))

(ert-deftest qss-present-repl-insert-file-name ()
  (qss-prtest--with-dir dir
    (qss-prtest--in-repl
      (qss-prtest--send (format "(files %S)" dir))
      (goto-char (point-max))
      (insert "!cat ")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (let ((o (cl-find-if (lambda (o) (string-suffix-p "new.txt" (qss-file-path (overlay-get o 'qss-object))))
                                        qss--accept-overlays)))
                     (qss--accept-choose o) ""))))
        (qss-repl-insert-file-name))
      (should (string-suffix-p (format "!cat %S" (abbreviate-file-name (concat dir "new.txt")))
                               (buffer-string))))))

;;; ------------------------------------------------------------------
;;; Commands

(ert-deftest qss-present-commands-in-menu ()
  "On a file row the menu offers the commands for files; they accept the rest."
  (qss-prtest--with-dir dir
    (save-window-excursion
      (let ((buf (qss-show (qss-files dir))))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "old.txt")
              (let ((labels (mapcar #'car (apply #'append (qss-actions-at-point)))))
                (should (member "Compare with file..." labels))
                (should (member "Copy into directory..." labels)))
              ;; Copy into directory: the target is clicked
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _)
                           (should (= 1 (length qss--accept-overlays))) ; only the directory
                           (qss--accept-choose (car qss--accept-overlays))
                           "")))
                (funcall (cdr (assoc "Copy into directory..."
                                     (apply #'append (qss-actions-at-point))))))
              (should (file-exists-p (expand-file-name "target/old.txt" dir)))
              ;; Compare with file: diff gets both names
              (let ((args nil))
                (cl-letf (((symbol-function 'diff) (lambda (&rest a) (setq args a)))
                          ((symbol-function 'completing-read)
                           (lambda (&rest _) (abbreviate-file-name (concat dir "new.txt")))))
                  (funcall (cdr (assoc "Compare with file..."
                                       (apply #'append (qss-actions-at-point))))))
                (should (equal args (list (concat dir "old.txt") (concat dir "new.txt"))))))
          (kill-buffer buf))))))

(ert-deftest qss-present-commands-on-values ()
  "A size cell offers the commands for sizes."
  (qss-prtest--with-dir dir
    (save-window-excursion
      (let ((buf (qss-show (qss-files dir))))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "old.txt")
              (while (not (eq (qss--column-at-point) 'size)) (forward-char 1))
              (should (assoc "Files larger than this, here"
                             (apply #'append (qss-actions-at-point)))))
          (kill-buffer buf))))))

(ert-deftest qss-present-define-command ()
  (let ((qss-commands nil))
    (eval '(qss-define-command qss-prtest--cmd "Test it" ((n integer "N: ") (s string "S: "))
             "A test command."
             (format "%s/%s" n s))
          t)
    (should (equal (cdr (assq 'qss-prtest--cmd qss-commands))
                   '("Test it" ((n integer "N: ") (s string "S: ")))))
    (should (equal (mapcar #'car (qss-commands-for 3 'integer)) '("Test it...")))
    (should (equal (mapcar #'car (qss-commands-for 3 'size)) '("Test it...")))
    (should-not (qss-commands-for "x" 'string))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "hello")))
      (should (equal (funcall (cdr (car (qss-commands-for 3 'integer)))) "3/hello")))
    (fmakunbound 'qss-prtest--cmd)))

;;; ------------------------------------------------------------------
;;; Presentations in a buffer of one's own (the example of the README)

(ert-deftest qss-present-own-buffer ()
  "Files presented in any buffer are offered by accept, like table rows."
  (qss-prtest--with-dir dir
    (save-window-excursion
      (let ((buf (get-buffer-create "*qss-test notes*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (erase-buffer)
                (insert "Files to look at:\n")
                (dolist (f (directory-files dir t "\\.txt\\'"))
                  (insert "  ")
                  (qss-present (list :path f) 'file (file-name-nondirectory f))
                  (insert "\n")))
              (switch-to-buffer buf)
              (should (= 2 (length (qss--visible-presentations 'file))))
              (should (null (qss--visible-presentations 'directory)))
              (let ((args nil))
                (cl-letf (((symbol-function 'diff) (lambda (&rest a) (setq args a)))
                          ((symbol-function 'completing-read) (qss-prtest--click-on "old.txt")))
                  (qss-compare-files (concat dir "new.txt")
                                     (qss-accept 'file "with file: ")))
                (should (equal args (list (concat dir "new.txt") (concat dir "old.txt"))))))
          (kill-buffer buf))))))

;;; lines

(ert-deftest qss-present-line-rows ()
  "A row of `lines' is a `line', below `object', labelled FILE:NUMBER."
  (let ((file (make-temp-file "qss-pline-" nil ".txt" "one\ntwo\n")))
    (unwind-protect
        (let ((row (cadr (qss-table-rows (qss-lines file)))))
          (should (eq (qss-presentation-type row) 'line))
          (should (qss-presentation-subtype-p 'line 'object))
          (should (equal (qss-presentation-label row)
                         (concat (file-name-nondirectory file) ":2"))))
      (delete-file file))))

(provide 'qss-present-test)
;;; qss-present-test.el ends here
