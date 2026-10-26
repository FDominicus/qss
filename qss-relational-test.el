;;; qss-relational-test.el --- Tests for join, group-by and summarize  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-relational-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defconst qss-reltest--orders
  '((:no 1 :cust 1 :amount 10.0 :status "paid")
    (:no 2 :cust 2 :amount 5.5 :status nil)
    (:no 3 :cust 1 :amount 20.0 :status "paid")
    (:no 4 :cust 9 :amount 1.0 :status "open")
    (:no 5 :cust nil :amount nil :status "open")))

(defconst qss-reltest--customers
  '((:cust 1 :name "Anna" :status "gold")
    (:cust 2.0 :name "Bob" :status nil)
    (:cust 3 :name "Clara" :status "new")))

(defmacro qss-reltest--with-db (var &rest body)
  "Bind VAR to an SQLite file with the tables orders and customers above."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let* ((dir (make-temp-file "qss-rel-" t))
            (,var (expand-file-name "rel.sqlite3" dir)))
       (unwind-protect
           (progn
             (qss-sql-write (qss-make-table "o" qss-reltest--orders) ,var "orders")
             (qss-sql-write (qss-make-table "c" (mapcar (lambda (r) (plist-put (copy-sequence r) :cust
                                                                                (let ((c (plist-get r :cust)))
                                                                                  (and c (truncate c)))))
                                                        qss-reltest--customers))
                            ,var "customers")
             ,@body)
         (delete-directory dir t)))))

(defun qss-reltest--rows (x)
  "Return the rows of X as property lists of all fields shown."
  (let ((tab (qss-as-table x)))
    (mapcar (lambda (r) (cl-mapcan (lambda (c) (list (car c) (qss-field r (car c))))
                                   (qss-table-columns tab)))
            (qss-table-rows tab))))

;;; ------------------------------------------------------------------
;;; join in memory

(ert-deftest qss-join-inner-and-names ()
  (let ((j (qss-> qss-reltest--orders (join qss-reltest--customers :on cust))))
    ;; status on both sides: ambiguous; the left one stays, using it is an error
    (should (equal (mapcar #'car (qss-table-columns j)) '(no cust amount status name)))
    (should-error (qss-> j (select no status)) :type 'user-error)
    (should-error (qss-> j (where (= :status "gold"))) :type 'user-error)
    (should-error (qss-> j (sort-by status)) :type 'user-error)
    (should-error (qss-collect j) :type 'user-error)
    ;; the other columns are plain
    (should (equal (qss-> j (select no name) collect) '((:no 1 :name "Anna") (:no 2 :name "Bob")
                                                       (:no 3 :name "Anna"))))
    ;; 2 and 2.0 match; nil never matches; 9 has no partner
    (should (equal (qss-> j (get no) collect) '(1 2 3)))
    ;; :suffix (or :prefix) renames the right columns but the join columns, as in Nushell
    (let ((s (qss-> qss-reltest--orders (join qss-reltest--customers :on cust :suffix "-c"))))
      (should (equal (mapcar #'car (qss-table-columns s)) '(no cust amount status name-c status-c)))
      (should (equal (qss-field (car (qss-table-rows s)) 'status-c) "gold")))
    (let ((s (qss-> qss-reltest--orders (join qss-reltest--customers :on cust :prefix "c-"))))
      (should (equal (mapcar #'car (qss-table-columns s)) '(no cust amount status c-name c-status))))))

(ert-deftest qss-join-left ()
  (let ((j (qss-> qss-reltest--orders (join qss-reltest--customers :on cust :kind left))))
    (should (equal (qss-> j (get no) collect) '(1 2 3 4 5)))
    (should (equal (qss-> j (where (is-null :name)) (get no) collect) '(4 5)))))

(ert-deftest qss-join-natural-and-pairs ()
  ;; without :on: all columns both have, here cust and status
  (should (equal (qss-> qss-reltest--orders (join qss-reltest--customers) (get no) collect) nil))
  ;; different names on both sides
  (let ((j (qss-> '((:id 1 :x "a") (:id 2 :x "b"))
                  (join '((:ref 2 :y "B") (:ref 1 :y "A") (:ref 1 :y "AA")) :on (id ref)))))
    (should (equal (qss-reltest--rows j)
                   '((id 1 x "a" ref 1 y "A") (id 1 x "a" ref 1 y "AA") (id 2 x "b" ref 2 y "B"))))))

(ert-deftest qss-join-errors ()
  (should-error (qss-> '((:a 1)) (join '((:b 1)))) :type 'user-error)
  (should-error (qss-> '((:a 1)) (join '((:a 1)) :on b)) :type 'user-error)
  (should-error (qss-> '((:a 1)) (join '((:a 1)) :kind outer)) :type 'user-error)
  (should-error (qss-> '((:a 1)) (join '((:a 1)) :how a)) :type 'user-error))

;;; ------------------------------------------------------------------
;;; group-by and summarize in memory

(ert-deftest qss-group-by-aggregates ()
  (let ((g (qss-> qss-reltest--orders
                  (group-by status (sum amount) (avg amount) (max no) (count amount) (min amount :as least)))))
    (should (equal (mapcar #'car (qss-table-columns g))
                   '(status count sum-amount avg-amount max-no count-amount least)))
    ;; sorted by key, missing last; NULL values are left out of the aggregates
    (should (equal (qss-reltest--rows g)
                   '((status "open" count 2 sum-amount 1.0 avg-amount 1.0 max-no 5 count-amount 1 least 1.0)
                     (status "paid" count 2 sum-amount 30.0 avg-amount 15.0 max-no 3 count-amount 2 least 10.0)
                     (status nil count 1 sum-amount 5.5 avg-amount 5.5 max-no 2 count-amount 1 least 5.5))))))

(ert-deftest qss-group-by-several-keys-and-ret ()
  (let ((g (qss-> qss-reltest--orders (group-by status cust))))
    (should (equal (mapcar (lambda (r) (list (qss-field r 'status) (qss-field r 'cust) (qss-field r 'count)))
                           (qss-table-rows g))
                   '(("open" 9 1) ("open" nil 1) ("paid" 1 2) (nil 2 1))))
    (save-window-excursion
      (with-current-buffer (qss-default-action (nth 2 (qss-table-rows g)))
        (unwind-protect
            (should (equal (qss-> qss--table (get no) collect) '(1 3)))
          (kill-buffer))))))

(ert-deftest qss-group-by-keeps-types ()
  (let* ((tab (qss-make-table "f" '((:k "a" :size 1000) (:k "a" :size 3000))
                              '(k (size :type size))))
         (g (qss-> tab (group-by k (sum size) (avg size) (count size)))))
    (should (eq 'size (plist-get (cdr (assq 'sum-size (qss-table-columns g))) :type)))
    (should (eq 'size (plist-get (cdr (assq 'avg-size (qss-table-columns g))) :type)))
    (should (eq 'number (plist-get (cdr (assq 'count-size (qss-table-columns g))) :type)))
    (should (string-match-p "3\\.9k" (qss-to g 'text)))))

(ert-deftest qss-summarize-memory ()
  (should (equal (qss-reltest--rows (qss-> qss-reltest--orders (summarize (sum amount) (max status))))
                 '((count 5 sum-amount 36.5 max-status "paid"))))
  ;; no rows: one row, as in SQL
  (should (equal (qss-reltest--rows (qss-> qss-reltest--orders (where (> :no 99)) (summarize (sum amount))))
                 '((count 0 sum-amount nil)))))

(ert-deftest qss-group-by-errors ()
  (let ((qss-contract-enabled nil))
    (should-error (qss-> qss-reltest--orders (group-by (sum amount))) :type 'user-error)
    (should-error (qss-> qss-reltest--orders (summarize status)) :type 'user-error)
    (should-error (qss-> qss-reltest--orders (group-by status (median amount))) :type 'user-error))
  ;; with the contracts, the last two stop at the boundary already
  (let ((qss-contract-enabled t))
    (should-error (qss-> qss-reltest--orders (summarize status)) :type 'qss-contract-violation)
    (should-error (qss-> qss-reltest--orders (group-by status (median amount)))
                  :type 'qss-contract-violation)))

;;; ------------------------------------------------------------------
;;; The same on SQLite

(ert-deftest qss-join-sql-same-as-memory ()
  (qss-reltest--with-db db
    (dolist (kind '(inner left))
      ;; the options of join are data in a pipe, so here the function
      (let ((sql (qss-sort-by (qss-join (qss-sqlite db "orders") (qss-sqlite db "customers")
                                        :on 'cust :kind kind)
                              'no))
            (mem (qss-sort-by (qss-join (qss-as-table (qss-sqlite db "orders"))
                                        (qss-as-table (qss-sqlite db "customers"))
                                        :on 'cust :kind kind)
                              'no)))
        (should (qss-relation-p sql))
        (should (string-match-p (if (eq kind 'left) "^ *LEFT JOIN (" "^ *JOIN (") (qss-sql sql)))
        (should (equal (qss-reltest--rows sql) (qss-reltest--rows mem)))))))

(ert-deftest qss-join-sql-goes-on-as-sql ()
  (qss-reltest--with-db db
    (let ((q (qss-> (sqlite db "orders") (join (sqlite db "customers") :on cust :suffix "-c")
                    (where (= :status-c "gold")) (select no name-c))))
      (should (qss-relation-p q))
      (should (string-search "WHERE (\"status-c\" = ?)" (qss-sql q)))
      (should (equal (qss-collect q) '((:no 1 :name-c "Anna") (:no 3 :name-c "Anna")))))
    ;; without :suffix status is ambiguous in SQL as in memory
    (should-error (qss-> (sqlite db "orders") (join (sqlite db "customers") :on cust)
                         (where (= :status "gold")))
                  :type 'user-error)))

(ert-deftest qss-join-other-file-in-memory ()
  (qss-reltest--with-db db
    (let ((j (qss-> (sqlite db "orders") (join qss-reltest--customers :on cust))))
      (should (qss-table-p j))
      (should (equal (qss-> j (get no) collect) '(1 2 3))))))

(ert-deftest qss-group-by-sql-same-as-memory ()
  (qss-reltest--with-db db
    (let ((args '((status (sum amount) (avg amount) (max no) (count amount))
                  (status cust)
                  (cust (min status)))))
      (dolist (a args)
        (let ((sql (apply #'qss-group-by (qss-sqlite db "orders") a))
              (mem (apply #'qss-group-by (qss-as-table (qss-sqlite db "orders")) a)))
          (should (qss-relation-p sql))
          (should (string-search "GROUP BY" (qss-sql sql)))
          (should (equal (qss-reltest--rows sql) (qss-reltest--rows mem))))))))

(ert-deftest qss-group-by-sql-having-and-ret ()
  (qss-reltest--with-db db
    (let ((g (qss-> (sqlite db "orders") (group-by status (sum amount)) (where (> :sum-amount 2)))))
      (should (qss-relation-p g))
      (should (equal (qss-> g (get status) collect) '("paid" nil)))
      ;; RET on a group: its rows, fetched with a query of its own
      (save-window-excursion
        (dolist (grp (qss-collect g))
          (with-current-buffer (qss-default-action grp)
            (unwind-protect
                (should (equal (qss-> qss--table (get no) collect)
                               (if (qss-field grp 'status) '(1 3) '(2))))
              (kill-buffer))))))))

(ert-deftest qss-group-by-over-join-sql ()
  (qss-reltest--with-db db
    (let ((g (qss-> (sqlite db "orders") (join (sqlite db "customers") :on cust)
                    (group-by name (sum amount)))))
      (should (qss-relation-p g))
      (should (equal (qss-reltest--rows g)
                     '((name "Anna" count 2 sum-amount 30.0) (name "Bob" count 1 sum-amount 5.5)))))))

(ert-deftest qss-summarize-sql ()
  (qss-reltest--with-db db
    (should (equal (qss-reltest--rows (qss-> (sqlite db "orders") (summarize (sum amount) (max status))))
                   '((count 5 sum-amount 36.5 max-status "paid"))))
    (should (equal (qss-reltest--rows (qss-> (sqlite db "orders") (where (> :no 99)) (summarize (sum amount))))
                   '((count 0 sum-amount nil))))))

(ert-deftest qss-group-by-sql-display ()
  "A grouped query is shown with its aggregates as numbers."
  (qss-reltest--with-db db
    (save-window-excursion
      (let ((buf (qss-display (qss-> (sqlite db "orders") (group-by status (sum amount))))))
        (unwind-protect
            (with-current-buffer buf
              (should (string-match-p "paid +2 +30\\.0" (buffer-string))))
          (kill-buffer buf))))))

(provide 'qss-relational-test)
;;; qss-relational-test.el ends here
