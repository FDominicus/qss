;;; qss-repl-test.el --- Tests for the qss REPL  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-repl-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defmacro qss-rtest--with-repl (&rest body)
  "Run BODY in a fresh qss REPL buffer, with `qss-last' unset."
  (declare (indent 0))
  `(let ((qss-last nil))
     (when (get-buffer "*qss*") (kill-buffer "*qss*"))
     (save-window-excursion
       (with-current-buffer (qss)
         (unwind-protect (progn ,@body)
           (kill-buffer "*qss*"))))))

(defun qss-rtest--send (input)
  "Type INPUT at the prompt and press RET; return the new output."
  (goto-char (point-max))
  (insert input)
  (let ((start (point)))
    (qss-repl-send)
    (buffer-substring-no-properties start (point-max))))

(defmacro qss-rtest--with-dir (var &rest body)
  "Bind VAR to a directory with three files, then run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "qss-repl-" t)))
     (unwind-protect
         (progn
           (dolist (f '(("a.el" . 100) ("b.el" . 3000) ("c.txt" . 50)))
             (with-temp-file (expand-file-name (car f) ,var)
               (insert (make-string (cdr f) ?x))))
           ,@body)
       (delete-directory ,var t))))

(ert-deftest qss-repl-pipe-shows-table ()
  (qss-rtest--with-dir dir
    (qss-rtest--with-repl
      (let ((out (qss-rtest--send (format "(files %S) (where (like :name \"%%.el\")) (sort-by size :desc)" dir))))
        (should (string-match-p "^2 rows  \\[buffer\\]  .*| where (like :name \"%.el\") | sort-by size desc$" out))
        (should (string-match-p "b\\.el.*\n.*a\\.el" out))
        (should-not (string-match-p "c\\.txt" out))
        (should (string-suffix-p qss-repl-prompt out))
        (should (qss-table-p qss-last))))))

(ert-deftest qss-repl-continues-last-table ()
  (qss-rtest--with-dir dir
    (qss-rtest--with-repl
      (qss-rtest--send (format "(files %S)" dir))
      (let ((out (qss-rtest--send "(where (> :size 1000))")))
        (should (string-match-p "^1 row  \\[buffer\\]  .*| where (> :size 1000)$" out))
        (should (string-match-p "b\\.el" out))
        (should-not (string-match-p "a\\.el" out)))
      ;; a bare word continues too
      (should (string-match-p "^1$" (qss-rtest--send "count"))))))

(ert-deftest qss-repl-lisp-and-errors ()
  (qss-rtest--with-repl
    (should (string-match-p "^3$" (qss-rtest--send "(+ 1 2)")))
    (should (string-match-p "^\"abc\"\\|^abc$" (qss-rtest--send "(concat \"a\" \"bc\")")))
    ;; without a last table, a word is Lisp: here an error, shown in the buffer
    (let ((out (qss-rtest--send "(where (> :size 1))")))
      (should (string-match-p "where" out))
      (should (string-suffix-p qss-repl-prompt out)))
    (let ((out (qss-rtest--send "(files \"/no/such/dir\")")))
      (should (string-match-p "No such file" out))
      (should (string-suffix-p qss-repl-prompt out)))))

(ert-deftest qss-repl-incomplete-input-waits ()
  (qss-rtest--with-repl
    (goto-char (point-max))
    (insert "(+ 1")
    (qss-repl-send)
    ;; no evaluation yet: the input goes on in the next line
    (should (string-match-p "(\\+ 1\n" (buffer-substring-no-properties qss-repl--input-start (point-max))))
    (insert "2)")
    (let ((start (point)))
      (qss-repl-send)
      (should (string-match-p "^3$" (buffer-substring-no-properties start (point-max)))))))

(ert-deftest qss-repl-long-table-is-cut ()
  (qss-rtest--with-repl
    (let* ((qss-repl-max-rows 5)
           (out (qss-rtest--send "(number-sequence 1 12) (each (list :n it :sq (* it it)))")))
      (should (string-match-p "12 rows" out))
      (should (string-match-p "\\.\\.\\. 7 more" out))
      (should (string-match-p "\\[show all\\]" out))
      (should (string-match-p "^5 +25 *$" out))
      (should-not (string-match-p " 36 *$" out))
      ;; the note comes after the rows, not between header and rows
      (should (< (string-match "^5 +25" out) (string-match "\\.\\.\\. 7 more" out))))))

(ert-deftest qss-repl-sqlite ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((temporary-file-directory (make-temp-file "qss-repl-db-" t)))
    (unwind-protect
        (qss-rtest--with-repl
          (let ((out (qss-rtest--send "(sqlite (qss-demo-database) \"Orders\") (where (> :amount 100)) (sort-by amount :desc)")))
            (should (string-match-p "4 rows" out))
            (should (string-match-p "4715.*999\\.99" out))
            (should (qss-relation-p qss-last)))
          ;; continuing a query stays a query
          (qss-rtest--send "(first 2)")
          (should (qss-relation-p qss-last))
          (should (string-search "LIMIT ?" (qss-rtest--send "sql"))))
      (delete-directory temporary-file-directory t))))

(ert-deftest qss-repl-history ()
  (qss-rtest--with-repl
    (qss-rtest--send "(+ 1 1)")
    (qss-rtest--send "(+ 2 2)")
    (qss-repl-previous-input)
    (should (equal (buffer-substring-no-properties qss-repl--input-start (point-max)) "(+ 2 2)"))
    (qss-repl-previous-input)
    (should (equal (buffer-substring-no-properties qss-repl--input-start (point-max)) "(+ 1 1)"))
    (qss-repl-next-input)
    (should (equal (buffer-substring-no-properties qss-repl--input-start (point-max)) "(+ 2 2)"))))

(ert-deftest qss-repl-completion ()
  (qss-rtest--with-dir dir
    (qss-rtest--with-repl
      (qss-rtest--send (format "(files %S)" dir))
      (goto-char (point-max))
      (insert "(sort-by mod")
      (let ((c (qss-repl-completion-at-point)))
        (should c)
        (should (member "modified" (nth 2 c)))
        (should (member ":modified" (nth 2 c)))
        (should (member "where" (nth 2 c)))
        (should (member "sqlite" (nth 2 c)))))))

(ert-deftest qss-repl-ret-on-row-opens-file ()
  (qss-rtest--with-dir dir
    (qss-rtest--with-repl
      (qss-rtest--send (format "(files %S) (where (= :name \"b.el\"))" dir))
      ;; the first row of the table (the input and the title mention b.el too)
      (goto-char (next-single-property-change (point-min) 'vtable-object))
      (should (qss-file-p (qss-current-row)))
      (let ((buf (save-window-excursion (qss-default-action (qss-current-row)) (current-buffer))))
        (should (equal (buffer-name buf) "b.el"))
        (kill-buffer buf)))))

(ert-deftest qss-repl-output-is-read-only ()
  (qss-rtest--with-repl
    (qss-rtest--send "(+ 1 2)")
    (goto-char (point-min))
    (should-error (insert "x") :type 'text-read-only)
    (goto-char (point-max))
    (insert "ok")
    (should (string-suffix-p "ok" (buffer-string)))))

(provide 'qss-repl-test)
;;; qss-repl-test.el ends here
