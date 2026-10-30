;;; qss-odbc-test.el --- Tests for qss-odbc  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Unit tests run always.  Database tests run against each DSN in the
;; environment variable QSS_ODBC_DSNS (separated by spaces, default
;; "test"); a DSN that cannot be reached, or a missing module, skips
;; them.  The tests create the table QSS_ODBC_T and drop it again.
;; Cases after plain-odbc (src/test/qss-tests.lisp), as far as they
;; concern the interface.

;;; Code:

(require 'ert)
(require 'qss-odbc)

(declare-function qss-odbc--exec "qss-odbc-module")
(declare-function qss-odbc--fetch "qss-odbc-module")
(declare-function qss-odbc--close "qss-odbc-module")
(declare-function qss-odbc--commit "qss-odbc-module")
(declare-function qss-odbc--rollback "qss-odbc-module")
(declare-function qss-odbc--set-autocommit "qss-odbc-module")

;;; ------------------------------------------------------------------
;;; Without a database

(ert-deftest qss-odbc-scaled-decimals ()
  (should (= (qss-odbc--scaled "1094.80" 2) 109480))
  (should (= (qss-odbc--scaled "1094.8" 2) 109480))
  (should (= (qss-odbc--scaled "-0.05" 2) -5))
  (should (= (qss-odbc--scaled ".5" 1) 5))
  (should (= (qss-odbc--scaled "12" 0) 12))
  (should (= (qss-odbc--scaled "1.000" 0) 1))
  (should (= (qss-odbc--scaled "123456789012345678901234567890.12" 2)
             12345678901234567890123456789012))
  (should-error (qss-odbc--scaled "1.25" 1) :type 'qss-odbc-error)
  (should-error (qss-odbc--scaled "1e5" 0) :type 'qss-odbc-error))

(ert-deftest qss-odbc-decimal-strings ()
  (should (equal (qss-odbc-decimal-string 109480 2) "1094.80"))
  (should (equal (qss-odbc-decimal-string -5 2) "-0.05"))
  (should (equal (qss-odbc-decimal-string 5 3) "0.005"))
  (should (equal (qss-odbc-decimal-string 0 2) "0.00"))
  (should (equal (qss-odbc-decimal-string 42 0) "42"))
  (dolist (n '(0 1 -1 99 100 -100 123456789 -987654321012345678901))
    (dolist (s '(0 1 2 4 6))
      (should (= (qss-odbc--scaled (qss-odbc-decimal-string n s) s) n)))))

(ert-deftest qss-odbc-parameters ()
  (should (equal (qss-odbc--param t) [bit 1]))
  (should (equal (qss-odbc-bool nil) [bit 0]))
  (should (equal (qss-odbc--param 42) 42))
  (should (equal (qss-odbc--param 9223372036854775807) 9223372036854775807))
  (should (equal (qss-odbc--param (1+ 9223372036854775807))
                 [decimal "9223372036854775808" 19 0]))
  (should (equal (qss-odbc-decimal 109480 2) [decimal "1094.80" 6 2]))
  (should (equal (qss-odbc--param "\377\0") [binary "\377\0"]))
  (should (equal (qss-odbc--param "abc") "abc"))
  (should (equal (qss-odbc-date "1776-07-04") [date 1776 7 4]))
  (should (equal (qss-odbc-timestamp "2026-03-29 02:30:00")
                 [timestamp 2026 3 29 2 30 0 0]))
  (should (equal (qss-odbc-timestamp "2026-10-30 09:56:00.25")
                 [timestamp 2026 10 30 9 56 0 250000000])))

(ert-deftest qss-odbc-time-strings ()
  (should (equal (qss-odbc--time-string [date 1899 12 30]) "1899-12-30"))
  (should (equal (qss-odbc--time-string [time 8 15 0]) "08:15:00"))
  (should (equal (qss-odbc--time-string [timestamp 2026 10 30 9 56 0 0])
                 "2026-10-30 09:56:00"))
  (should (equal (qss-odbc--time-string [timestamp 2026 10 30 9 56 0 123456000])
                 "2026-10-30 09:56:00.123456"))
  ;; Wall clock: the change to summer time does not move 02:30.
  (should (equal (format-time-string "%F %T" (qss-odbc-parse-time "2026-03-29 02:30:00") t)
                 "2026-03-29 02:30:00")))

(ert-deftest qss-odbc-ansi-drivers ()
  (should (qss-odbc--ansi-utf-8-driver-p "psqlodbca.so"))
  (should (qss-odbc--ansi-utf-8-driver-p "/usr/lib/x86_64-linux-gnu/odbc/libsqlite3odbc.so"))
  (should-not (qss-odbc--ansi-utf-8-driver-p "psqlodbcw.so")))

(ert-deftest qss-odbc-hides-password ()
  (should (equal (qss-odbc--hide-password "DSN=x;UID=a;PWD=geheim;Port=1")
                 "DSN=x;UID=a;PWD=***;Port=1")))

;;; ------------------------------------------------------------------
;;; Against databases

(defun qss-odbc-test--dsns ()
  "The DSNs to test against."
  (split-string (or (getenv "QSS_ODBC_DSNS") "test")))

(defvar qss-odbc-test--connections nil
  "DSN -> connection or :unreachable, kept across tests.")

(defun qss-odbc-test--connection (dsn)
  "A connection to DSN, or nil if it cannot be reached."
  (let ((c (cdr (assoc dsn qss-odbc-test--connections))))
    (cond ((eq c :unreachable) nil)
          ((and c (qss-odbc-connected-p c)) c)
          (t (let ((new (condition-case err
                            (qss-odbc-connect dsn)
                          (error (message "qss-odbc-test: %s unreachable: %s"
                                          dsn (error-message-string err))
                                 :unreachable))))
               (setf (alist-get dsn qss-odbc-test--connections nil nil #'equal) new)
               (and (not (eq new :unreachable)) new))))))

(defun qss-odbc-test--sqlite-p (con)
  (string-match-p "sqlite" (qss-odbc-connection-dbms con)))

(defun qss-odbc-test--postgres-p (con)
  (string-match-p "postgres" (downcase (qss-odbc-connection-dbms con))))

(defun qss-odbc-test--type (con kind)
  "Column type for KIND in the dialect of CON."
  (let ((pg (qss-odbc-test--postgres-p con)))
    (pcase kind
      ('text "TEXT")
      ('varchar "VARCHAR(255)")
      ('int "INTEGER")
      ('bigint "BIGINT")
      ('double "DOUBLE PRECISION")
      ('money "NUMERIC(12,2)")
      ('big "NUMERIC(40,4)")
      ('bool (if pg "BOOLEAN" "BIT"))
      ('date "DATE")
      ('timestamp (if pg "TIMESTAMP(6)" "TIMESTAMP"))
      ('blob (if pg "BYTEA" "BLOB")))))

(defmacro qss-odbc-deftest (name doc &rest body)
  "Define test NAME that runs BODY with `con' bound, for each DSN.
In BODY, `dsn' is the DSN.  Without module or reachable DSN: skip."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,name ()
     ,doc
     (unless (qss-odbc-available-p)
       (message "%s: skipped, ODBC module not built (M-x qss-odbc-build-module)" ',name))
     (skip-unless (qss-odbc-available-p))
     (let ((ran nil))
       (dolist (dsn (qss-odbc-test--dsns))
         (let ((con (qss-odbc-test--connection dsn)))
           (when con
             (setq ran t)
             (ert-info ((format "DSN %s (%s)" dsn (qss-odbc-connection-dbms con)))
               ,@body))))
       (unless ran
         (message "%s: skipped, no DSN of %S reachable" ',name (qss-odbc-test--dsns)))
       (skip-unless ran))))

(defun qss-odbc-test--table (con &rest columns)
  "Create QSS_ODBC_T on CON with COLUMNS: (NAME KIND) ...; drop it first."
  (qss-odbc-execute con "DROP TABLE IF EXISTS qss_odbc_t")
  (qss-odbc-execute
   con (format "CREATE TABLE qss_odbc_t (%s)"
               (mapconcat (lambda (c) (format "%s %s" (car c)
                                              (qss-odbc-test--type con (cadr c))))
                          columns ", "))))

(defun qss-odbc-test--drop (con)
  (qss-odbc-execute con "DROP TABLE IF EXISTS qss_odbc_t"))

(defun qss-odbc-test--roundtrip (con kind value)
  "Write VALUE into a column of KIND and read it back."
  (qss-odbc-test--table con (list 'v kind))
  (unwind-protect
      (progn
        (should (= 1 (qss-odbc-execute con "INSERT INTO qss_odbc_t (v) VALUES (?)"
                                       value)))
        (car (nth 1 (qss-odbc-query con "SELECT v FROM qss_odbc_t"))))
    (qss-odbc-test--drop con)))

(qss-odbc-deftest qss-odbc-db-literals
    "Values written in the SQL come back with their types."
  (let ((r (qss-odbc-query con "SELECT 1+1 AS n, 'Müller' AS name, NULL AS nix, 2.5 AS f")))
    (should (equal (car r) '("n" "name" "nix" "f")))
    (should (equal (nth 0 (nth 1 r)) 2))
    (should (equal (nth 1 (nth 1 r)) "Müller"))
    (should (null (nth 2 (nth 1 r))))
    (should (null (nth 2 r))))
  ;; Text beyond Latin-1 in the SQL itself (SQLite: through UTF-8)
  (should (equal (cadr (qss-odbc-query con "SELECT 'Ωx 😀 日本' AS t"))
                 '("Ωx 😀 日本"))))

(qss-odbc-deftest qss-odbc-db-text
    "Text through parameters: empty, NULL, umlauts, scripts, surrogate pairs."
  (dolist (s '("" "a" "Größe €" "\\ ' \"" "Ελληνικά" "देवनागरी" "العربية"
               "日本語" "Кириллица" "😀 x 🎉" "Zeile 1\nZeile 2\r\n\tTab"))
    (ert-info ((format "%S" s))
      (should (equal (qss-odbc-test--roundtrip con 'text s) s))
      (should (equal (qss-odbc-test--roundtrip con 'varchar s) s))))
  (should (null (qss-odbc-test--roundtrip con 'text nil))))

(qss-odbc-deftest qss-odbc-db-long-text
    "Long text, with surrogate pairs on the piece boundaries of SQLGetData."
  (dolist (n '(16382 16383 16384 32767 32768 100000))
    (let ((s (concat (make-string (- n 1) ?a) "😀" (make-string 5000 ?ü) "😀")))
      (ert-info ((format "%d" n))
        (should (equal (qss-odbc-test--roundtrip con 'text s) s))))))

(qss-odbc-deftest qss-odbc-db-integers
    "Integers at the limits of 32 and 64 bit."
  (dolist (n '(0 1 -1 2147483647 -2147483648))
    (should (eql (qss-odbc-test--roundtrip con 'int n) n)))
  (dolist (n '(2147483648 -2147483649 9223372036854775807 -9223372036854775808))
    (should (eql (qss-odbc-test--roundtrip con 'bigint n) n))))

(qss-odbc-deftest qss-odbc-db-floats
    "Doubles."
  (dolist (x '(0.0 1.5 -2.25 1e100 3.141592653589793 -1.0000000000000002))
    (let ((back (qss-odbc-test--roundtrip con 'double x)))
      (if (qss-odbc-test--sqlite-p con)
          ;; The SQLite driver keeps 15 digits (plain-odbc: DRIVER-ROUNDS-DOUBLES)
          (should (< (abs (- back x)) (* 1e-14 (max 1 (abs x)))))
        (should (= back x))))))

(qss-odbc-deftest qss-odbc-db-decimals
    "NUMERIC comes as an integer scaled by the scale of the column."
  (unless (qss-odbc-test--sqlite-p con) ; SQLite has no exact numbers
  (should (eql (qss-odbc-test--roundtrip con 'money (qss-odbc-decimal 109480 2)) 109480))
  (should (eql (qss-odbc-test--roundtrip con 'money (qss-odbc-decimal -5 2)) -5))
  (should (eql (qss-odbc-test--roundtrip con 'money 1094) 109400))
  (should (= (qss-odbc-test--roundtrip
              con 'big (qss-odbc-decimal 1234567890123456789012345678901234 4))
             1234567890123456789012345678901234))
  ;; beyond 64 bit as a plain integer parameter
  (should (= (qss-odbc-test--roundtrip con 'big (expt 10 30)) (* (expt 10 30) 10000)))
  (let ((col (car (car (qss-odbc-select con "SELECT CAST(1.5 AS NUMERIC(8,3)) AS x")))))
    (should (eq (qss-odbc-column-type col) 'numeric))
    (should (eql (qss-odbc-column-scale col) 3)))))

(qss-odbc-deftest qss-odbc-db-booleans
    "t and (qss-odbc-bool nil)."
  (should (eq (qss-odbc-test--roundtrip con 'bool t) t))
  (should (eq (qss-odbc-test--roundtrip con 'bool (qss-odbc-bool nil)) nil)))

(qss-odbc-deftest qss-odbc-db-dates
    "Dates and timestamps as wall clock text; before 1900, summer time."
  (unless (qss-odbc-test--sqlite-p con)  ; SQLite keeps what it gets
  (should (equal (qss-odbc-test--roundtrip con 'date (qss-odbc-date "1776-07-04"))
                 "1776-07-04"))
  (should (equal (qss-odbc-test--roundtrip con 'date (qss-odbc-date "1899-12-30"))
                 "1899-12-30"))
  (dolist (ts '("2026-03-29 02:30:00" "2026-10-25 02:30:00" "1899-12-30 08:15:00"
                "2026-10-30 09:56:00.123456"))
    (should (equal (qss-odbc-test--roundtrip con 'timestamp (qss-odbc-timestamp ts))
                   ts)))
  (should (equal (qss-odbc-test--roundtrip
                  con 'timestamp
                  (qss-odbc-timestamp (qss-odbc-parse-time "2026-10-30 09:56:00")))
                 "2026-10-30 09:56:00"))))

(qss-odbc-deftest qss-odbc-db-binary
    "Binary data: all byte values, empty, long."
  (let ((all (apply #'unibyte-string (number-sequence 0 255)))
        (long (apply #'unibyte-string
                     (mapcar (lambda (i) (% (* i 7) 256)) (number-sequence 0 99999)))))
    (should (equal (qss-odbc-test--roundtrip con 'blob (qss-odbc-binary all)) all))
    (should (equal (qss-odbc-test--roundtrip con 'blob (qss-odbc-binary long)) long))
    (should (equal (qss-odbc-test--roundtrip con 'blob (qss-odbc-binary "")) ""))))

(qss-odbc-deftest qss-odbc-db-errors
    "An error carries SQLSTATE and diagnostics; the connection stays usable."
  (let ((err (should-error (qss-odbc-query con "SELECT * FROM gibt_es_nicht")
                           :type 'qss-odbc-error)))
    (should (stringp (qss-odbc-error-message err)))
    (should (string-match-p "gibt_es_nicht" (qss-odbc-error-message err)))
    (should (= (length (qss-odbc-error-sqlstate err)) 5))
    (should (consp (qss-odbc-error-diagnostics err)))
    (when (qss-odbc-test--postgres-p con)
      (should (equal (qss-odbc-error-sqlstate err) "42P01"))))
  (should (equal (cadr (qss-odbc-query con "SELECT 1")) '(1)))
  ;; A long message (psqlODBC splits them), with umlauts
  (when (qss-odbc-test--postgres-p con)
    (let* ((name (concat (make-string 3000 ?x) "äöü"))
           (err (should-error
                 (qss-odbc-query con "SELECT CAST(? AS INTEGER)" name)
                 :type 'qss-odbc-error)))
      (should (string-match-p "x\\{3000\\}äöü" (qss-odbc-error-message err))))))

(qss-odbc-deftest qss-odbc-db-transactions
    "Rollback undoes, commit keeps; autocommit is back afterwards."
  (qss-odbc-test--table con '(n int))
  (unwind-protect
      (progn
        (should-error
         (qss-odbc-with-transaction con
           (qss-odbc-execute con "INSERT INTO qss_odbc_t VALUES (1)")
           (error "abort")))
        (should (equal (cadr (qss-odbc-query con "SELECT count(*) FROM qss_odbc_t")) '(0)))
        (qss-odbc-with-transaction con
          (qss-odbc-execute con "INSERT INTO qss_odbc_t VALUES (2)"))
        (should (equal (cadr (qss-odbc-query con "SELECT count(*) FROM qss_odbc_t")) '(1)))
        ;; autocommit again: this one stays without commit
        (qss-odbc-execute con "INSERT INTO qss_odbc_t VALUES (3)")
        (qss-odbc-rollback con)
        (should (equal (cadr (qss-odbc-query con "SELECT count(*) FROM qss_odbc_t")) '(2))))
    (qss-odbc-test--drop con)))

(qss-odbc-deftest qss-odbc-db-row-counts
    "Rows affected, also 0."
  (qss-odbc-test--table con '(n int))
  (unwind-protect
      (progn
        (dotimes (i 5) (qss-odbc-execute con "INSERT INTO qss_odbc_t VALUES (?)" i))
        (should (= (qss-odbc-execute con "UPDATE qss_odbc_t SET n = n + 10 WHERE n < 3") 3))
        (should (= (qss-odbc-execute con "DELETE FROM qss_odbc_t WHERE n > 100") 0)))
    (qss-odbc-test--drop con)))

(qss-odbc-deftest qss-odbc-db-row-by-row
    "do-query: one row at a time, cl-return, error in the body."
  (qss-odbc-test--table con '(n int) '(s text))
  (unwind-protect
      (progn
        (qss-odbc-with-transaction con
          (dotimes (i 2000)
            (qss-odbc-execute con "INSERT INTO qss_odbc_t VALUES (?, ?)" i (format "z%d" i))))
        (let ((sum 0) (count 0))
          (qss-odbc-do-query ((n s) con "SELECT n, s FROM qss_odbc_t WHERE n >= ?" 0)
            (should (equal s (format "z%d" n)))
            (cl-incf sum n) (cl-incf count))
          (should (= count 2000))
          (should (= sum (/ (* 1999 2000) 2))))
        (should (equal (qss-odbc-do-query ((n) con "SELECT n FROM qss_odbc_t ORDER BY n")
                         (when (= n 7) (cl-return 'found)))
                       'found))
        (should-error (qss-odbc-do-query ((n) con "SELECT n FROM qss_odbc_t")
                        (error "in the body %s" n)))
        (should (equal (cadr (qss-odbc-query con "SELECT count(*) FROM qss_odbc_t")) '(2000))))
    (qss-odbc-test--drop con)))

(qss-odbc-deftest qss-odbc-db-many-statements
    "Many queries without closing: the garbage collector frees them."
  (dotimes (i 3000)
    (qss-odbc--exec (qss-odbc-connection-handle con) "SELECT ?" (vector i)))
  (garbage-collect)
  (should (equal (cadr (qss-odbc-query con "SELECT 42")) '(42))))

(qss-odbc-deftest qss-odbc-db-many-parameters
    "100 parameters in one statement."
  (let* ((n 100)
         (sql (concat "SELECT " (mapconcat (lambda (i) (format "? AS p%d" i))
                                          (number-sequence 1 n) ", ")))
         (vals (number-sequence 1 n))
         (row (cadr (apply #'qss-odbc-query con sql vals))))
    (should (equal (mapcar (lambda (v) (if (stringp v) (string-to-number v) v)) row)
                   vals))))

(qss-odbc-deftest qss-odbc-db-catalog
    "Tables, columns and primary key through the catalog functions."
  (qss-odbc-execute con "DROP TABLE IF EXISTS qss_odbc_t")
  (qss-odbc-execute con "CREATE TABLE qss_odbc_t (id INTEGER PRIMARY KEY, name VARCHAR(40))")
  (unwind-protect
      (progn
        (should (member "qss_odbc_t"
                        (mapcar (lambda (r) (downcase (plist-get r :table_name)))
                                (qss-odbc-tables con "TABLE"))))
        (should (equal (mapcar (lambda (r) (downcase (plist-get r :column_name)))
                               (qss-odbc-table-columns con "qss_odbc_t"))
                       '("id" "name")))
        (should (equal (mapcar #'downcase (qss-odbc-primary-key con "qss_odbc_t"))
                       '("id"))))
    (qss-odbc-test--drop con)))

(qss-odbc-deftest qss-odbc-db-more-results
    "Several result sets in one statement (PostgreSQL)."
  (when (qss-odbc-test--postgres-p con)
   (let ((sets (qss-odbc-select-all con "SELECT 1 AS a; SELECT 'x' AS b, 2 AS c")))
    (should (= (length sets) 2))
    (should (equal (cdr (nth 0 sets)) '((1))))
    (should (equal (cdr (nth 1 sets)) '(("x" 2)))))))

(qss-odbc-deftest qss-odbc-db-closed-objects
    "A closed statement or connection gives an error, not a crash."
  (let ((stmt (qss-odbc--exec (qss-odbc-connection-handle con) "SELECT 1")))
    (qss-odbc--close stmt)
    (should-error (qss-odbc--fetch stmt) :type 'qss-odbc-error))
  (let* ((c2 (qss-odbc-connect dsn))
         (stmt (qss-odbc--exec (qss-odbc-connection-handle c2) "SELECT 1")))
    (qss-odbc-disconnect c2)
    (should-not (qss-odbc-connected-p c2))
    (should-error (qss-odbc--fetch stmt) :type 'qss-odbc-error)
    (should-error (qss-odbc-query c2 "SELECT 1") :type 'qss-odbc-error)
    (should-error (qss-odbc--fetch (qss-odbc-connection-handle con)) :type 'qss-odbc-error)
    (garbage-collect)))

(qss-odbc-deftest qss-odbc-db-second-connection
    "A second connection sees committed rows."
  (qss-odbc-test--table con '(n int))
  (unwind-protect
      (let ((c2 (qss-odbc-connect dsn)))
        (unwind-protect
            (progn
              (qss-odbc-execute con "INSERT INTO qss_odbc_t VALUES (7)")
              (should (equal (cadr (qss-odbc-query c2 "SELECT n FROM qss_odbc_t")) '(7))))
          (qss-odbc-disconnect c2)))
    (qss-odbc-test--drop con)))

(qss-odbc-deftest qss-odbc-db-info
    "Information about the connection."
  (should (stringp (qss-odbc-connection-dbms con)))
  (should (member (qss-odbc-connection-quote con) '("\"" "`" "[" " ")))
  (should (string-match-p "#<qss-odbc " (cl-prin1-to-string con))))

(provide 'qss-odbc-test)
;;; qss-odbc-test.el ends here
