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
      ;; the message is in the language of the system; the path is in it either way
      (should (string-match-p "/no/such/dir" out))
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

;;; The REPL as a shell

(defmacro qss-rtest--with-tree (var &rest body)
  "Bind VAR to a directory with log.txt, src/ and \"sub dir/\", then run BODY."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "qss-shell-" t))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "src" ,var))
           (make-directory (expand-file-name "sub dir" ,var))
           (with-temp-file (expand-file-name "log.txt" ,var)
             (insert "alpha error one\nbeta ok\ngamma error two\n"))
           ,@body)
       (delete-directory ,var t))))

(ert-deftest qss-repl-split-words ()
  (should (equal (qss-repl-split-words "sh -c \"echo out; echo err >&2; exit 3\"")
                 '("sh" "-c" "echo out; echo err >&2; exit 3")))
  (should (equal (qss-repl-split-words "echo \"a  b\" 'it s'") '("echo" "a  b" "it s")))
  (should (equal (qss-repl-split-words "grep \"x\\\"y\" f") '("grep" "x\"y" "f")))
  (should (equal (qss-repl-split-words "a\\ b c") '("a b" "c")))
  (should (equal (qss-repl-split-words "printf %s;x | y") '("printf" "%s;x" "|" "y")))
  (should (equal (qss-repl-split-words "  ") nil))
  (should-error (qss-repl-split-words "echo 'open") :type 'user-error))

(ert-deftest qss-repl-cd-and-prompt ()
  (qss-rtest--with-tree dir
    (qss-rtest--with-repl
      (qss-rtest--send (concat "cd " dir))
      (should (equal default-directory dir))
      (should (string-suffix-p (concat (abbreviate-file-name dir) " qss> ") (buffer-string)))
      (qss-rtest--send "cd sub dir")                 ; a space in the name, no quotes
      (should (equal default-directory (expand-file-name "sub dir/" dir)))
      (qss-rtest--send "cd ..")
      (should (equal default-directory dir))
      (qss-rtest--send "cd \"src\"")
      (should (equal default-directory (expand-file-name "src/" dir)))
      (qss-rtest--send "cd")
      (should (equal default-directory (file-name-as-directory (expand-file-name "~"))))
      (should (string-match-p "No such directory" (qss-rtest--send "cd /no/such/dir/qss")))
      ;; Lisp cd works too, and the sources run there
      (qss-rtest--send (format "(cd %S)" dir))
      (should (equal (qss-> (files) (where (= :name "log.txt")) (get name) collect) '("log.txt"))))))

(ert-deftest qss-repl-run-program ()
  (skip-unless (executable-find "grep"))
  (qss-rtest--with-tree dir
    (qss-rtest--with-repl
      (qss-rtest--send (concat "cd " dir))
      (let ((out (qss-rtest--send "!grep error log.txt")))
        (should (string-match-p "\\`\nalpha error one\ngamma error two\n" out)))
      ;; the output is the last table, one row per line
      (should (equal (qss-collect qss-last) '((:line "alpha error one") (:line "gamma error two"))))
      (should (string-match-p "1 row" (qss-rtest--send "(where (like :line \"%two%\"))")))
      (should (string-match-p "No such file\\|Searching for program\\|no-such-program-qss"
                              (qss-rtest--send "!no-such-program-qss"))))))

(ert-deftest qss-repl-run-exit-status ()
  (skip-unless (executable-find "sh"))
  (qss-rtest--with-repl
    (let ((out (qss-rtest--send "!sh -c \"echo out; echo err >&2; exit 3\"")))
      (should (string-match-p "^out$" out))
      (should (string-match-p "^err$" out))          ; standard error mixed in
      (should (string-match-p "^\\[exit 3\\]$" out)))))

(ert-deftest qss-run-source ()
  (skip-unless (executable-find "sh"))
  (should (equal (qss-> (run "sh" "-c" "printf 'b\\na\\nc\\n'") (sort-by line) (get line) collect)
                 '("a" "b" "c")))
  (should (string-match-p "\\[exit 2\\]\\'" (qss-table-title (qss-run "sh" "-c" "exit 2")))))

(ert-deftest qss-repl-ret-enters-directory ()
  (qss-rtest--with-tree dir
    (qss-rtest--with-repl
      (qss-rtest--send (concat "cd " dir))
      (qss-rtest--send "(files) (where (= :type \"dir\")) (sort-by name)")
      (goto-char (point-max))
      (re-search-backward "^sub dir")
      (qss--default (qss-current-row))
      (should (equal default-directory (expand-file-name "sub dir/" dir)))
      (should (string-match-p (concat "qss> cd " (regexp-quote (abbreviate-file-name (expand-file-name "sub dir" dir))))
                              (buffer-string)))
      (should (string-match-p "0 rows .*files .*sub dir/" (buffer-string))))))

(ert-deftest qss-repl-file-completion ()
  (qss-rtest--with-tree dir
    (qss-rtest--with-repl
      (qss-rtest--send (concat "cd " dir))
      (dolist (case '(("cd s" "sub dir/" "src/") ("!cat l" "log.txt") ("(files \"s" "sub dir/" "src/")))
        (goto-char (point-max))
        (let ((inhibit-read-only t)) (delete-region qss-repl--input-start (point-max)))
        (insert (car case))
        (let* ((c (qss-repl-completion-at-point))
               (found (all-completions (buffer-substring-no-properties (nth 0 c) (nth 1 c)) (nth 2 c))))
          (should (equal (sort found #'string<) (sort (copy-sequence (cdr case)) #'string<))))))))

(provide 'qss-repl-test)
;;; qss-repl-test.el ends here
