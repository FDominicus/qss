;;; qss-sql-odbc-test.el --- Tests for ODBC as a source of qss  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The shared cases run in qss-spec-test.el (`qss-spec-cases-odbc').
;; Here what they do not cover.  Against the DSNs of QSS_ODBC_DSNS
;; (default "test"); creates and drops the table qss_sql_odbc_t.

;;; Code:

(require 'ert)
(require 'qss)

(declare-function qss-odbc-execute "qss-odbc")
(declare-function qss-odbc-available-p "qss-odbc")

(defmacro qss-sql-odbc-test--each (dsn &rest body)
  "Run BODY with DSN bound to each reachable DSN; skip if there is none.
Use it directly in the body of `ert-deftest' (it uses `skip-unless')."
  (declare (indent 1))
  `(progn
     (skip-unless (and (require 'qss-odbc nil t) (qss-odbc-available-p)))
     (let ((ran nil))
       (dolist (,dsn (split-string (or (getenv "QSS_ODBC_DSNS") "test")))
         (when (condition-case nil (qss-sql-odbc-database ,dsn) (error nil))
           (setq ran t)
           (ert-info ((format "DSN %s" ,dsn)) ,@body)))
       (unless ran (message "qss-sql-odbc-test: skipped, no DSN reachable"))
       (skip-unless ran))))

(defun qss-sql-odbc-test--names ()
  "A small table of names: code points and a language order differ."
  (qss-make-table "names"
                  (mapcar (lambda (n) (list :nr (car n) :name (cdr n)))
                          '((1 . "Anna") (2 . "bert") (3 . "Carla") (4 . "Ärger") (5 . "Zoe")))
                  '(nr name)))

(ert-deftest qss-sql-odbc-text-by-code-points ()
  "sort-by and < > on text compare code points, whatever the collation."
  (qss-sql-odbc-test--each dsn
     (qss-> (qss-sql-odbc-test--names) (into odbc dsn "qss_sql_odbc_t" :replace))
     (unwind-protect
         (progn
           (should (equal (qss-collect (qss-> (odbc dsn "qss_sql_odbc_t") (sort-by :name) (get :nr)))
                          '(1 3 5 2 4)))
           ;; "bert" and "Ärger" come after "Z" by code points
           (should (equal (qss-collect (qss-> (odbc dsn "qss_sql_odbc_t") (where (> :name "Z"))
                                              (sort-by :nr) (get :nr)))
                          '(2 4 5)))
           (should (equal (qss-collect (qss-> (odbc dsn "qss_sql_odbc_t") (where (< :name "a"))
                                              (sort-by :nr) (get :nr)))
                          '(1 3 5))))
       (qss-odbc-execute (qss-sql-odbc-connection (qss-sql-odbc-database dsn))
                         "DROP TABLE IF EXISTS qss_sql_odbc_t")
       (qss-odbc-refresh dsn))))

(ert-deftest qss-sql-odbc-tables-and-into ()
  "(odbc DSN) lists the tables, into writes and appends, the rows come back."
  (qss-sql-odbc-test--each dsn
     (qss-> (qss-sql-odbc-test--names) (into odbc dsn "qss_sql_odbc_t" :replace))
     (unwind-protect
         (progn
           (should (member "qss_sql_odbc_t"
                           (mapcar (lambda (r) (downcase (qss-db-table-name r)))
                                   (qss-table-rows (qss-odbc-source dsn)))))
           (should-error (qss-> (qss-sql-odbc-test--names) (into odbc dsn "qss_sql_odbc_t")))
           (qss-> (qss-sql-odbc-test--names) (into odbc dsn "qss_sql_odbc_t" :append))
           (should (= (qss-> (odbc dsn "qss_sql_odbc_t") (count)) 10))
           (should (equal (qss-> (odbc dsn "qss_sql_odbc_t") (where (= :nr 4)) (first) (get :name))
                          "Ärger")))
       (qss-odbc-execute (qss-sql-odbc-connection (qss-sql-odbc-database dsn))
                         "DROP TABLE IF EXISTS qss_sql_odbc_t")
       (qss-odbc-refresh dsn))))

(provide 'qss-sql-odbc-test)
;;; qss-sql-odbc-test.el ends here
