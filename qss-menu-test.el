;;; qss-menu-test.el --- Tests for the actions at point  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-menu-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defmacro qss-mtest--with-db (&rest body)
  "Run BODY with a fresh demo database in a temporary directory."
  (declare (indent 0))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let ((temporary-file-directory (make-temp-file "qss-menu-" t)))
       (unwind-protect (progn ,@body)
         (delete-directory temporary-file-directory t)))))

(defmacro qss-mtest--with-repl (&rest body)
  "Run BODY in a fresh qss REPL buffer, with `qss-last' unset."
  (declare (indent 0))
  `(let ((qss-last nil))
     (when (get-buffer "*qss*") (kill-buffer "*qss*"))
     (save-window-excursion
       (with-current-buffer (qss)
         (unwind-protect (progn ,@body)
           (kill-buffer "*qss*"))))))

(defmacro qss-mtest--with-dir (var &rest body)
  "Bind VAR to a directory with two files, then run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "qss-menu-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "small.txt" ,var) (insert "one\n"))
           (with-temp-file (expand-file-name "large.txt" ,var) (insert (make-string 5000 ?x)))
           ,@body)
       (delete-directory ,var t))))

(defun qss-mtest--send (input)
  "Type INPUT at the prompt of the REPL and press RET."
  (goto-char (point-max))
  (insert input)
  (qss-repl-send))

(defun qss-mtest--goto-cell (text column)
  "Go to the cell in COLUMN (a symbol) of the last row line containing TEXT."
  (goto-char (point-max))
  (search-backward text)
  (beginning-of-line)
  (let ((end (line-end-position)))
    (while (and (< (point) end) (not (eq (qss--column-at-point) column)))
      (forward-char 1))
    (should (eq (qss--column-at-point) column))))

(defun qss-mtest--labels ()
  "Return the labels of the actions at point."
  (mapcar #'car (apply #'append (qss-actions-at-point))))

(defun qss-mtest--run (label)
  "Run the action at point called LABEL."
  (let ((action (assoc label (apply #'append (qss-actions-at-point)))))
    (should action)
    (funcall (cdr action))))

;;; ------------------------------------------------------------------

(ert-deftest qss-menu-value-stages ()
  "The words offered depend on the value."
  (should (equal (car (qss--value-stages 'status nil '(status)))
                 '(where (is-null :status))))
  (should (member '(where (= :status "paid")) (qss--value-stages 'status "paid" '(status))))
  (should (member '(where (> :amount 120.0)) (qss--value-stages 'amount 120.0 '(amount))))
  (should-not (member '(where (> :status "paid")) (qss--value-stages 'status "paid" '(status))))
  (should (member '(where (= :kind 'dir)) (qss--value-stages 'kind 'dir '(kind))))
  ;; a list cannot be written in a condition: only the column words
  (should (equal (car (qss--value-stages 'x '("a" "b") '(x))) '(sort-by x)))
  ;; a time: the day, not the exact moment
  (let ((stages (qss--value-stages 'modified (encode-time '(0 30 23 31 12 2026 nil -1 nil))
                                   '(modified :type time))))
    (should (equal (car stages)
                   '(where (and (>= :modified "2026-12-31") (< :modified "2027-01-01")))))))

(ert-deftest qss-menu-repl-where-becomes-sql ()
  "In the REPL, a value word is submitted as input and stays SQL."
  (qss-mtest--with-db
    (qss-mtest--with-repl
      (qss-mtest--send "(sqlite (qss-demo-database) \"Orders\")")
      (qss-mtest--goto-cell "4713" 'status)
      (should (member "(where (is-null :status))" (qss-mtest--labels)))
      (qss-mtest--run "(where (is-null :status))")
      (should (qss-relation-p qss-last))
      (should (string-search "IS NULL" (qss-sql qss-last)))
      (should (= 2 (qss-count qss-last)))
      (should (equal (car qss-repl--history) "(where (is-null :status))"))
      (should (string-search "qss> (where (is-null :status))\n2 rows" (buffer-string))))))

(ert-deftest qss-menu-repl-keeps-typed-input ()
  "What is typed at the prompt stays there after an action."
  (qss-mtest--with-db
    (qss-mtest--with-repl
      (qss-mtest--send "(sqlite (qss-demo-database) \"Orders\")")
      (goto-char (point-max))
      (insert "(first 2")
      (qss-mtest--goto-cell "4711" 'status)
      (qss-mtest--run "(where (= :status \"paid\"))")
      (should (= 3 (qss-count qss-last)))
      (should (string-suffix-p "qss> (first 2" (buffer-string))))))

(ert-deftest qss-menu-repl-older-table ()
  "An action on an older table continues that table, not the newest."
  (qss-mtest--with-db
    (qss-mtest--with-repl
      (qss-mtest--send "(sqlite (qss-demo-database) \"Orders\")")
      (qss-mtest--send "(sqlite (qss-demo-database) \"Customers\")")
      (goto-char (point-min))
      (search-forward "4715")
      (while (not (eq (qss--column-at-point) 'amount)) (forward-char 1))
      (qss-mtest--run "(where (> :amount 999.99))")
      (should (string-search "Orders" (qss-relation-table qss-last)))
      (should (= 0 (qss-count qss-last))))))

(ert-deftest qss-menu-show-buffer ()
  "Outside the REPL, the result opens in a buffer of its own."
  (qss-mtest--with-dir dir
    (save-window-excursion
      (let ((buf (qss-show (qss-files dir))))
        (unwind-protect
            (with-current-buffer buf
              (qss-mtest--goto-cell "small.txt" 'name)
              (let ((result (qss-mtest--run "(where (= :name \"small.txt\"))")))
                (unwind-protect
                    (with-current-buffer result
                      (should (= 1 (length (qss-table-rows qss--table))))
                      ;; a time: the day of the file
                      (qss-mtest--goto-cell "small.txt" 'modified)
                      (let ((day (format-time-string "%Y-%m-%d"
                                                     (qss-field (qss-current-row) 'modified))))
                        (should (member (format "(where (>= :modified %S))" day)
                                        (qss-mtest--labels)))))
                  (kill-buffer result))))
          (kill-buffer buf))))))

(ert-deftest qss-menu-sql-buffer-continues-query ()
  "A query shown in a buffer is continued as a query."
  (qss-mtest--with-db
    (save-window-excursion
      (let ((buf (qss-display (qss-sqlite (qss-demo-database) "Orders"))))
        (unwind-protect
            (with-current-buffer buf
              (should (qss-relation-p (qss-table-at-point)))
              (qss-mtest--goto-cell "4716" 'status)
              (let ((result (qss-mtest--run "(where (= :status \"cancelled\"))")))
                (unwind-protect
                    (with-current-buffer result
                      (should (qss-relation-p qss--whole))
                      (should (string-search "WHERE" (qss-sql qss--whole))))
                  (kill-buffer result))))
          (kill-buffer buf))))))

(ert-deftest qss-menu-copy-whole-table ()
  "w copies all rows, also those the REPL does not show."
  (qss-mtest--with-db
    (qss-mtest--with-repl
      (let ((qss-repl-max-rows 2) (kill-ring nil) (qss-copy-format 'csv))
        (qss-mtest--send "(sqlite (qss-demo-database) \"Orders\")")
        (qss-mtest--goto-cell "4712" 'order-no)
        (should (eq (key-binding (kbd "w")) #'qss-copy-table))
        (qss-copy-table)
        (should (= 7 (length (split-string (car kill-ring) "\n" t))))
        (should (string-prefix-p "order-no,customer-no,order-date,amount,status\n4711,1,2026-09-01,120.0,paid\n"
                                 (car kill-ring)))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "org")))
          (qss-copy-table t))
        (should (string-prefix-p "| order-no |" (car kill-ring)))))))

(ert-deftest qss-menu-copy-value ()
  (qss-mtest--with-db
    (qss-mtest--with-repl
      (let ((kill-ring nil))
        (qss-mtest--send "(sqlite (qss-demo-database) \"Customers\")")
        (qss-mtest--goto-cell "Clara" 'name)
        (qss-mtest--run "Copy value of name")
        (should (equal (car kill-ring) "Clara Weiss"))))))

(ert-deftest qss-menu-popup ()
  "The right mouse button pops up the same actions, in groups."
  (qss-mtest--with-db
    (qss-mtest--with-repl
      (qss-mtest--send "(sqlite (qss-demo-database) \"Orders\")")
      (qss-mtest--goto-cell "4713" 'status)
      (should (eq (key-binding [mouse-3]) #'qss-context-menu))
      (let ((pos (point)) (shown nil))
        (cl-letf (((symbol-function 'mouse-set-point) (lambda (_) (goto-char pos)))
                  ((symbol-function 'popup-menu) (lambda (map &rest _) (setq shown map))))
          (qss-context-menu nil))
        (let ((items (delq nil (mapcar (lambda (b) (and (eq (car-safe (cdr-safe b)) 'menu-item)
                                                        (nth 2 b)))
                                       (cdr shown)))))
          (should (equal (car items) "Describe"))
          (should (member "--" items))
          (should (member "(where (is-null :status))" items))
          (should (equal (car (last items)) "Copy table as tsv")))))))

(ert-deftest qss-menu-action-completion-keeps-order ()
  (qss-mtest--with-db
    (qss-mtest--with-repl
      (qss-mtest--send "(sqlite (qss-demo-database) \"Orders\")")
      (qss-mtest--goto-cell "4713" 'status)
      (let ((seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _)
                     (setq seen (funcall table "" nil 'metadata))
                     "(sort-by status :desc)")))
          (qss-action-at-point))
        (should (eq 'identity (alist-get 'display-sort-function (cdr seen))))
        (should (equal (car qss-repl--history) "(sort-by status :desc)"))))))

(provide 'qss-menu-test)
;;; qss-menu-test.el ends here
