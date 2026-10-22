;;; qss-examples-test.el --- Tests that the examples still work  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-examples-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss-examples)

(defmacro qss-etest--with-fresh-db (&rest body)
  "Run BODY with a demo database created anew in a temporary directory.
`user-emacs-directory', which the file examples read, is a directory
with a few files there, so the tests do not depend on ~/.emacs.d."
  (declare (indent 0))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let* ((temporary-file-directory (make-temp-file "qss-examples-" t))
            (user-emacs-directory (file-name-as-directory
                                   (expand-file-name "emacs.d" temporary-file-directory))))
       (make-directory user-emacs-directory)
       (dolist (f '(("init.el" . 6000) ("early-init.el" . 100) ("notes.txt" . 10)))
         (with-temp-file (expand-file-name (car f) user-emacs-directory)
           (insert (make-string (cdr f) ?x))))
       (unwind-protect (progn ,@body)
         (delete-directory temporary-file-directory t)))))

(defun qss-etest--value (title)
  "Return the result of the example called TITLE."
  (let ((example (assoc title qss-examples)))
    (should example)
    (eval (cadr example) t)))

(ert-deftest qss-examples-all-run ()
  "Every example runs without an error."
  (qss-etest--with-fresh-db
    (dolist (example (cl-remove-if (lambda (e) (and (cddr e) (not (eval (nth 2 e) t))))
                                   qss-examples))
      (let ((value (eval (cadr example) t)))
        (when (or (qss-table-p value) (qss-relation-p value))
          (should (listp (qss-collect value))))))))

(ert-deftest qss-examples-results ()
  "The SQLite examples return what their comments say."
  (qss-etest--with-fresh-db
    (should (equal (qss-etest--value "Any list of property lists is a table")
                   '((:name "Ada" :born 1815) (:name "Grace" :born 1906))))
    (should (equal (qss-etest--value "NULL as in SQL: (not (= ...)) does not keep rows without a value")
                   '("Bob Stone")))
    (should (equal (mapcar (lambda (r) (plist-get r :order-no))
                           (qss-collect (qss-etest--value "SQLite: a pipe becomes one query")))
                   '(4715 4713 4716 4711)))
    (should (string-search "WHERE (\"Amount\" > ?)" (qss-etest--value "SQLite: the SQL of that pipe")))
    (should (equal (qss-etest--value "SQLite: Lisp values become parameters, lists work with in")
                   '(4711 4713 4714 4716)))
    (should (= 2 (qss-etest--value "SQLite: open orders, i.e. Status is NULL")))
    (should (equal (qss-etest--value "SQLite: like ignores case, as in SQL")
                   '("Anna Meier" "Clara Weiss")))
    (should (equal (qss-etest--value "SQLite: first SQL, then Lisp with it (fetched rows, in memory)")
                   '("Bob Stone" "Dan Brown")))
    (should (equal (mapcar (lambda (g) (list (qss-field g 'status) (qss-field g 'count)))
                           (qss-collect (qss-etest--value "SQLite: orders per status (group-by runs in memory)")))
                   '(("paid" 3) (nil 2) ("cancelled" 1))))
    (should (= 245.5 (qss-etest--value "Using the rows in Lisp: the total of the paid orders")))
    (should (equal (car (qss-collect (qss-etest--value "Joining by hand: orders with the customer's name")))
                   '(:order-no 4711 :name "Anna Meier" :amount 120.0)))
    (should (equal (qss-etest--value "Storing: a pipe into SQLite, and on from there as a query")
                   '(4715 4713)))
    (should (string-prefix-p "| name        | city     | since      |\n|-------------+----------+------------|\n| Anna Meier  | Bruchsal | 2019-03-01 |\n"
                             (qss-etest--value "Text: a table as Org, to paste into a note (w in a table does the same)")))
    (should (string-match-p "\\`name,size,modified\n\\(.*,[0-9]+,[0-9-]+ [0-9:]+\n\\)\\{3\\}\\'"
                            (qss-etest--value "Text: CSV with the plain values, for other programs")))))

(ert-deftest qss-examples-buffer ()
  "M-x qss-examples shows every example and its result."
  (qss-etest--with-fresh-db
    (with-current-buffer (qss-examples)
      (unwind-protect
          (progn
            (dolist (example qss-examples)
              (should (string-search (car example) (buffer-string))))
            (should-not (string-search "(error " (buffer-string)))
            (should (string-search "[show]" (buffer-string))))
        (kill-buffer)))))

(provide 'qss-examples-test)
;;; qss-examples-test.el ends here
