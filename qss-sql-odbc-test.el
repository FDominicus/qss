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
(require 'qss-spec-test)                ; qss-spec-test--odbc-ready-p
(require 'qss-repl-test)                ; qss-rtest--with-repl (a macro)

(declare-function qss-odbc-execute "qss-odbc")
(declare-function qss-odbc-query "qss-odbc")
(defvar qss-contract-enabled)
(defvar qss-last)
(declare-function qss-odbc-parse-time "qss-odbc")
(declare-function qss-odbc-table-columns "qss-odbc")
(declare-function qss-odbc-connection-dbms "qss-odbc")
(declare-function qss-odbc-available-p "qss-odbc")

(defmacro qss-sql-odbc-test--each (dsn &rest body)
  "Run BODY with DSN bound to each reachable DSN; skip if there is none.
Use it directly in the body of `ert-deftest' (it uses `skip-unless')."
  (declare (indent 1))
  `(progn
     (skip-unless (qss-spec-test--odbc-ready-p 'qss-sql-odbc))
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

(ert-deftest qss-sql-odbc-into-times ()
  "into odbc stores times as TIMESTAMP (DATETIME), conditions compare them."
  (qss-sql-odbc-test--each dsn
     ;; qss-time: Emacs 32 gives whole seconds as a plain integer,
     ;; which qss would take for a number
     (let ((rows (list (list :nr 1 :when (qss-time (qss-odbc-parse-time "2026-01-15 08:30:00")))
                       (list :nr 2 :when (qss-time (qss-odbc-parse-time "2026-10-31 12:00:00")))
                       (list :nr 3 :when nil))))
       ;; parse-time gives UTC; into writes local time, as for SQLite
       (qss-> (qss-make-table "times" rows '(nr when)) (into odbc dsn "qss_sql_odbc_t" :replace))
       (unwind-protect
           (let* ((con (qss-sql-odbc-connection (qss-sql-odbc-database dsn)))
                  (type (plist-get (cl-find "when" (qss-odbc-table-columns con "qss_sql_odbc_t")
                                            :key (lambda (c) (plist-get c :column_name))
                                            :test #'equal)
                                   :type_name)))
             (unless (string-match-p "sqlite" (downcase (qss-odbc-connection-dbms con)))
               (should (string-match-p "timestamp\\|datetime" (downcase type))))
             (should (equal (qss-collect (qss-> (odbc dsn "qss_sql_odbc_t")
                                                (where (> :when "2026-06-01")) (get :nr)))
                            '(2)))
             (should (equal (qss-collect (qss-> (odbc dsn "qss_sql_odbc_t")
                                                (sort-by :when) (get :nr)))
                            '(1 2 3))))
         (qss-odbc-execute (qss-sql-odbc-connection (qss-sql-odbc-database dsn))
                           "DROP TABLE IF EXISTS qss_sql_odbc_t")
         (qss-odbc-refresh dsn)))))

(ert-deftest qss-sql-odbc-exact-numbers ()
  "Floats in conditions compare with NUMERIC as written; AVG as in memory.
psqlODBC sent 0.99 as \"0.98999999999999999\", so (= :p 0.99) found
nothing on NUMERIC(10,2); MariaDB rounded AVG of DECIMAL to 6 digits."
  (qss-sql-odbc-test--each dsn
     (let ((con (qss-sql-odbc-connection (qss-sql-odbc-database dsn))))
       (qss-odbc-execute con "DROP TABLE IF EXISTS qss_sql_odbc_t")
       (qss-odbc-execute con "CREATE TABLE qss_sql_odbc_t (id INTEGER PRIMARY KEY, p NUMERIC(10,2), d DOUBLE PRECISION)")
       (qss-odbc-execute con "INSERT INTO qss_sql_odbc_t VALUES (1, 0.99, 0.99), (2, 2.00, 2.0), (3, 2.00, 2.0), (4, -0.10, -0.1)")
       (qss-odbc-refresh dsn)
       (unwind-protect
           (let ((src (lambda () (qss-odbc-source dsn "qss_sql_odbc_t"))))
             (should (equal (qss-collect (qss-> (funcall src) (where (= :p 0.99)) (get :id))) '(1)))
             (should (equal (qss-collect (qss-> (funcall src) (where (= :d 0.99)) (get :id))) '(1)))
             (should (equal (qss-collect (qss-> (funcall src) (where (= :p -0.1)) (get :id))) '(4)))
             (should (equal (qss-collect (qss-> (funcall src) (where (> :p 0.99)) (sort-by :id) (get :id))) '(2 3)))
             (should (equal (qss-collect (qss-> (funcall src) (where (< :p 1.5)) (sort-by :id) (get :id))) '(1 4)))
             ;; (0.99 + 2 + 2 - 0.1) / 4 = 1.2225; without row 4: 4.99 / 3
             (should (< (abs (- (car (qss-collect (qss-> (funcall src) (where (> :id 0)) (summarize (avg :p)) (get :avg-p))))
                                1.2225))
                        1e-12))
             (should (< (abs (- (car (qss-collect (qss-> (funcall src) (where (< :id 4)) (summarize (avg :p)) (get :avg-p))))
                                (/ 4.99 3)))
                        1e-12)))
         (qss-sql-odbc-test--drop dsn "qss_sql_odbc_t")))))

;;; Editing

(defun qss-sql-odbc-test--sql (dsn sql &rest params)
  "Rows of SQL on DSN, straight from the database."
  (cdr (apply #'qss-odbc-query (qss-sql-odbc-connection (qss-sql-odbc-database dsn)) sql params)))

(defun qss-sql-odbc-test--drop (dsn &rest tables)
  (dolist (tab tables)
    (qss-odbc-execute (qss-sql-odbc-connection (qss-sql-odbc-database dsn))
                      (format "DROP %s IF EXISTS %s" (if (string-suffix-p "_v" tab) "VIEW" "TABLE") tab)))
  (qss-odbc-refresh dsn))

(ert-deftest qss-sql-odbc-update-and-delete-rows ()
  "update-rows and delete-rows on a table made by into (no primary key)."
  (qss-sql-odbc-test--each dsn
     (qss-> (qss-sql-odbc-test--names) (into odbc dsn "qss_sql_odbc_t" :replace))
     (unwind-protect
         (progn
           (should (= 2 (qss-> (odbc dsn "qss_sql_odbc_t") (where (> :nr 3))
                               (update-rows :name "neu"))))
           (should (equal (qss-sql-odbc-test--sql dsn "SELECT nr FROM qss_sql_odbc_t WHERE name = 'neu' ORDER BY nr")
                          '((4) (5))))
           ;; first and sort-by count
           (should (= 1 (qss-> (odbc dsn "qss_sql_odbc_t") (sort-by :nr :desc) (first 1)
                               (update-rows :name "letzte"))))
           (should (equal (qss-sql-odbc-test--sql dsn "SELECT nr FROM qss_sql_odbc_t WHERE name = 'letzte'")
                          '((5))))
           (should (= 1 (qss-> (odbc dsn "qss_sql_odbc_t") (where (= :nr 1)) (delete-rows))))
           (should (= 4 (qss-> (odbc dsn "qss_sql_odbc_t") (count))))
           (should-error (qss-> (odbc dsn "qss_sql_odbc_t") (delete-rows)) :type 'user-error))
       (qss-sql-odbc-test--drop dsn "qss_sql_odbc_t"))))

(ert-deftest qss-sql-odbc-edit-in-buffer ()
  "e, d and C-c C-c in a table buffer of an ODBC table with a primary key."
  (qss-sql-odbc-test--each dsn
     (let ((con (qss-sql-odbc-connection (qss-sql-odbc-database dsn))))
       (qss-sql-odbc-test--drop dsn "qss_sql_odbc_v" "qss_sql_odbc_t")
       (qss-odbc-execute con "CREATE TABLE qss_sql_odbc_t (nr INTEGER PRIMARY KEY, name VARCHAR(40), amount DOUBLE PRECISION)")
       (dolist (r '((1 "Anna" 10.0) (2 "bert" 20.0) (3 "Carla" 30.0)))
         (apply #'qss-odbc-execute con "INSERT INTO qss_sql_odbc_t VALUES (?, ?, ?)" r))
       (qss-odbc-refresh dsn)
       (unwind-protect
           (save-window-excursion
             (let ((buf (qss-display (qss-odbc-source dsn "qss_sql_odbc_t"))))
               (unwind-protect
                   (with-current-buffer buf
                     (should (equal (qss-sql-rowid (car (qss-table-rows qss--table))) '(1)))
                     (goto-char (point-min))
                     (search-forward "Anna")
                     (beginning-of-line)
                     (while (not (eq (qss--column-at-point) 'amount)) (forward-char 1))
                     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "12.5")))
                       (qss-edit-cell))
                     (goto-char (point-min))
                     (search-forward "bert")
                     (qss-edit-toggle-delete)
                     ;; the same value again: MariaDB counts 0 changed rows
                     (goto-char (point-min))
                     (search-forward "Carla")
                     (beginning-of-line)
                     (while (not (eq (qss--column-at-point) 'name)) (forward-char 1))
                     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Carla")))
                       (qss-edit-cell))
                     (qss-edit-commit)
                     (should-not (qss-edit-pending-p))
                     (should (equal (qss-sql-odbc-test--sql dsn "SELECT nr, amount FROM qss_sql_odbc_t ORDER BY nr")
                                    '((1 12.5) (3 30.0))))
                     (should (= 2 (length (qss-table-rows qss--table)))))
                 (with-current-buffer buf (setq qss-edit--changes nil))
                 (kill-buffer buf)))
             ;; a view cannot be changed
             (qss-odbc-execute con "CREATE VIEW qss_sql_odbc_v AS SELECT * FROM qss_sql_odbc_t")
             (qss-odbc-refresh dsn)
             (should-error (qss-> (odbc dsn "qss_sql_odbc_v") (where (= :nr 1)) (delete-rows))
                           :type 'user-error))
         (qss-sql-odbc-test--drop dsn "qss_sql_odbc_v" "qss_sql_odbc_t")))))

;;; In the REPL

(ert-deftest qss-sql-odbc-in-repl ()
  "(odbc ...) as a source in M-x qss, and a line continuing it."
  (qss-sql-odbc-test--each dsn
     (qss-> (qss-sql-odbc-test--names) (into odbc dsn "qss_sql_odbc_t" :replace))
     (unwind-protect
         (qss-rtest--with-repl
           (let ((out (qss-rtest--send (format "(odbc %S \"qss_sql_odbc_t\") (where (> :nr 3))" dsn))))
             (should (string-match-p "Zoe" out))
             (should-not (string-match-p "Anna" out))
             (should (qss-relation-p qss-last)))
           ;; the next line goes on with the query: still SQL
           (qss-rtest--send "(sort-by nr :desc)")
           (should (qss-relation-p qss-last))
           (should (equal (qss-collect (qss-> qss-last (get :nr))) '(5 4))))
       (qss-sql-odbc-test--drop dsn "qss_sql_odbc_t"))))

;;; Schemas and logging in

(ert-deftest qss-sql-odbc-schemas ()
  "A table of the same name in another schema is SCHEMA.TABLE (PostgreSQL)."
  (qss-sql-odbc-test--each dsn
     (when (string-match-p "postgres" (qss-sql-odbc--dbms (qss-sql-odbc-database dsn)))
       (let ((con (qss-sql-odbc-connection (qss-sql-odbc-database dsn))))
         (qss-odbc-execute con "DROP SCHEMA IF EXISTS qss_s CASCADE")
         (qss-odbc-execute con "CREATE SCHEMA qss_s")
         (qss-odbc-execute con "CREATE TABLE qss_s.qss_sql_odbc_t (id INTEGER PRIMARY KEY, other TEXT)")
         (qss-odbc-execute con "INSERT INTO qss_s.qss_sql_odbc_t VALUES (7, 'dort')")
         (qss-> (qss-sql-odbc-test--names) (into odbc dsn "qss_sql_odbc_t" :replace))
         (unwind-protect
             (progn
               ;; both, each with its own columns (not mixed)
               (should (equal (qss-collect (qss-> (odbc dsn "qss_sql_odbc_t") (columns)))
                              '(nr name)))
               (should (equal (qss-collect (qss-> (odbc dsn "qss_s.qss_sql_odbc_t") (columns)))
                              '(id other)))
               (should (equal (qss-collect (qss-> (odbc dsn "qss_s.qss_sql_odbc_t") (get :other)))
                              '("dort")))
               (should (string-match-p "\"qss_s\".\"qss_sql_odbc_t\""
                                       (qss-sql (qss-odbc-source dsn "qss_s.qss_sql_odbc_t"))))
               ;; and editable by its primary key
               (should (= 1 (qss-> (odbc dsn "qss_s.qss_sql_odbc_t") (where (= :id 7))
                                   (update-rows :other "hier"))))
               (should (equal (qss-sql-odbc-test--sql dsn "SELECT other FROM qss_s.qss_sql_odbc_t")
                              '(("hier"))))
               ;; into the current schema, not confused by the other
               (should (= 5 (qss-> (odbc dsn "qss_sql_odbc_t") (count)))))
           (qss-odbc-execute con "DROP SCHEMA IF EXISTS qss_s CASCADE")
           (qss-sql-odbc-test--drop dsn "qss_sql_odbc_t"))))))

(ert-deftest qss-sql-odbc-source-arguments ()
  "(odbc DSN :user ...) without a table: the options are options."
  (skip-unless (qss-spec-test--odbc-ready-p 'qss-sql-odbc-source-arguments))
  (let (got)
    (cl-letf (((symbol-function 'qss-sql-odbc-database)
               (lambda (dsn &rest options) (setq got (cons dsn options)) 'db))
              ((symbol-function 'qss-sql-db-tables) (lambda (_db) 'tables))
              ((symbol-function 'qss-sql-relation) (lambda (_db table) (list 'rel table))))
      (let ((qss-contract-enabled nil))
        (should (eq (qss-odbc-source "x" :user "u" :password "p") 'tables))
        (should (equal got '("x" :user "u" :password "p")))
        (should (equal (qss-odbc-source "x" "t" :user "u") '(rel "t")))
        (should (equal got '("x" :user "u")))
        (should (equal (qss-odbc-source "x" 'orders) '(rel "orders")))))))

(provide 'qss-sql-odbc-test)
;;; qss-sql-odbc-test.el ends here
