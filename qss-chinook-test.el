;;; qss-chinook-test.el --- The same pipes on the same data, every way  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Chinook (https://github.com/lerocha/chinook-database, MIT, see
;; test/chinook/LICENSE.md; changed as test/chinook/CHANGES.md says) is
;; one database --- a music shop: artists, albums, tracks, customers,
;; invoices --- with the same tables and rows for SQLite, PostgreSQL,
;; MariaDB/MySQL and more.  The pipes below run
;; on it in every way qss has, and every way must give what qss gives in
;; memory:
;;
;;   memory         the rows as Lisp data, no SQL
;;   sqlite         Emacs' own SQLite
;;   sqlite-odbc    the same file through the SQLite3 ODBC driver
;;                  (`qss-chinook-test-sqlite-driver'), if installed
;;   each DSN of QSS_CHINOOK_DSNS, e.g. "chinook_pg chinook_my"
;;
;; The SQLite file is built from test/chinook/Chinook_Sqlite.sql into a
;; temporary directory on first use.  The databases of the DSNs are
;; loaded by hand from the scripts there (they begin with DROP DATABASE);
;; the tests only read them.
;;
;; Table names differ (Track, track; InvoiceLine, invoice_line) and are
;; found by their qss name; column names become the same keys anyway.

;;; Code:

(require 'ert)
(require 'qss)
(require 'qss-spec-test)                ; qss-spec-test--odbc-ready-p

(declare-function qss-odbc-connect "qss-odbc")
(declare-function qss-odbc-disconnect "qss-odbc")

(defvar qss-chinook-test-sqlite-driver "SQLite3"
  "The name of the SQLite ODBC driver (`odbcinst -q -d').")

(defconst qss-chinook-test--dir
  (expand-file-name "test/chinook/"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "The directory of the Chinook scripts.")

(defconst qss-chinook-test--tables
  '("Album" "Artist" "Customer" "Employee" "Genre" "Invoice" "InvoiceLine"
    "MediaType" "Playlist" "PlaylistTrack" "Track")
  "The tables of Chinook, as SQLite names them.")

;;; ------------------------------------------------------------------
;;; The SQLite file

(defvar qss-chinook-test--file nil "The SQLite file, once built.")

(defun qss-chinook-test--statements (sql)
  "Split the script SQL into statements: at `;' outside quotes and comments."
  (let ((i 0) (n (length sql)) (start 0) (out nil))
    (while (< i n)
      (let ((c (aref sql i)))
        (cond ((eq c ?')                ; a string; '' is a quote in it
               (setq i (1+ i))
               (while (and (< i n) (not (and (eq (aref sql i) ?')
                                             (not (and (< (1+ i) n) (eq (aref sql (1+ i)) ?'))))))
                 (setq i (if (eq (aref sql i) ?') (+ i 2) (1+ i)))))
              ((and (eq c ?/) (< (1+ i) n) (eq (aref sql (1+ i)) ?*))
               (setq i (or (string-search "*/" sql (+ i 2)) n))
               (setq i (1+ i)))
              ((and (eq c ?-) (< (1+ i) n) (eq (aref sql (1+ i)) ?-))
               (setq i (or (string-search "\n" sql i) n)))
              ((eq c ?\;)
               (push (substring sql start i) out)
               (setq start (1+ i)))))
      (setq i (1+ i)))
    (push (substring sql start) out)
    ;; leave out what is only comments and space (the decision only:
    ;; the statements keep their comments)
    (mapcar #'string-trim
            (seq-filter (lambda (s)
                          (string-match-p
                           "[A-Za-z]"
                           (replace-regexp-in-string
                            "--[^\n]*" ""
                            (replace-regexp-in-string "/\\*\\(?:.\\|\n\\)*?\\*/" "" s))))
                        (nreverse out)))))

(defun qss-chinook-test--file ()
  "The Chinook SQLite file, built from the script on first use."
  (or (and qss-chinook-test--file (file-exists-p qss-chinook-test--file)
           qss-chinook-test--file)
      (let* ((dir (make-temp-file "qss-chinook" t))
             (file (expand-file-name "chinook.sqlite" dir))
             (sql (with-temp-buffer
                    (insert-file-contents (expand-file-name "Chinook_Sqlite.sql"
                                                            qss-chinook-test--dir))
                    (buffer-string)))
             (db (sqlite-open file)))
        (unwind-protect
            (progn
              (sqlite-transaction db)
              (dolist (s (qss-chinook-test--statements sql))
                (sqlite-execute db s))
              (sqlite-commit db))
          (sqlite-close db))
        (setq qss-chinook-test--file file))))

;;; ------------------------------------------------------------------
;;; The ways

(defvar qss-chinook-test--memory nil "Alist of (TABLE . ROWS), read once.")

(defun qss-chinook-test--memory (table)
  "The rows of TABLE as plists, read once from the SQLite file."
  (or (cdr (assoc table qss-chinook-test--memory))
      (let ((rows (qss-collect (qss-sqlite (qss-chinook-test--file) table))))
        (push (cons table rows) qss-chinook-test--memory)
        rows)))

(defun qss-chinook-test--odbc-table (dsn table)
  "The name of TABLE (\"InvoiceLine\") in the database of DSN (\"invoice_line\")."
  (let ((key (qss-sql-name-key table)))
    (or (cl-find key (mapcar (lambda (r) (plist-get r :table_name))
                             (qss-sql-odbc--tables (qss-sql-odbc-database dsn) "TABLE"))
                 :key #'qss-sql-name-key)
        table)))

(defun qss-chinook-test--source (way table)
  "The code of the source TABLE in WAY: memory, sqlite or a DSN."
  (pcase way
    ('memory `',(qss-chinook-test--memory table))
    ('sqlite `(qss-sqlite ,(qss-chinook-test--file) ,table))
    (_ `(qss-odbc-source ,way ,(qss-chinook-test--odbc-table way table)))))

(defun qss-chinook-test--sqlite-odbc ()
  "The connection string of the SQLite file through ODBC, or nil (and why)."
  (let ((cs (format "Driver=%s;Database=%s" qss-chinook-test-sqlite-driver
                    (qss-chinook-test--file))))
    (condition-case err
        (progn (qss-odbc-disconnect (qss-odbc-connect cs)) cs)
      (error (message "qss-chinook: SQLite through ODBC skipped (driver %s): %s"
                      qss-chinook-test-sqlite-driver (error-message-string err))
             nil))))

(defun qss-chinook-test--ways ()
  "The ways to compare with memory: sqlite, then the reachable ODBC ones."
  (append
   '(sqlite)
   (when (qss-spec-test--odbc-ready-p 'qss-chinook)
     (delq nil
           (cons (qss-chinook-test--sqlite-odbc)
                 (mapcar (lambda (dsn)
                           (condition-case err (progn (qss-sql-odbc-database dsn) dsn)
                             (error (message "qss-chinook: %s unreachable: %s"
                                             dsn (error-message-string err))
                                    nil)))
                         (split-string (or (getenv "QSS_CHINOOK_DSNS") ""))))))))

;;; ------------------------------------------------------------------
;;; The pipes

(defconst qss-chinook-test--cases
  '(("tracks" "Track" ((count)))
    ("artists by name, by code points" "Artist" ((sort-by :name :artist-id) (first 25) (get :name)))
    ("artists by name, descending" "Artist" ((sort-by :name :desc) (first 10) (get :name)))
    ("like folds ASCII only" "Artist" ((where (like :name "%the%")) (sort-by :artist-id) (get :name)))
    ("like with a letter beyond ASCII" "Customer"
     ((where (like :first-name "%ø%")) (sort-by :customer-id) (get :first-name)))
    ("text in many scripts" "Customer"
     ((where (> :customer-id 0)) (sort-by :last-name :customer-id) (select :customer-id :first-name :last-name :city)))
    ("long tracks" "Track"
     ((where (> :milliseconds 1000000)) (sort-by :track-id) (select :track-id :name :milliseconds :bytes)))
    ("exact prices" "Track" ((where (> :unit-price 1)) (count)))
    ("price = 0.99" "Track" ((where (= :unit-price 0.99)) (count)))
    ("prices of a genre" "Track" ((where (= :genre-id 20)) (sort-by :track-id) (get :unit-price)))
    ("no composer" "Track" ((where (= :composer nil)) (count)))
    ("composer, NULL last" "Track" ((sort-by :composer :track-id) (last 5) (get :track-id)))
    ("composer descending, NULL first" "Track" ((sort-by :composer :desc :track-id) (first 5) (get :track-id)))
    ("invoices since 2025" "Invoice" ((where (>= :invoice-date "2025-01-01")) (count)))
    ("invoice dates" "Invoice" ((sort-by :invoice-date :desc :invoice-id) (first 5) (select :invoice-id :invoice-date :total)))
    ("no state" "Invoice" ((where (is-null :billing-state)) (count)))
    ;; groups are objects in memory and rows in SQL: compared by get
    ("countries" "Invoice" ((group-sum :billing-country) (get :billing-country)))
    ("invoices per country" "Invoice" ((group-sum :billing-country) (get :count)))
    ("totals per country" "Invoice" ((group-sum :billing-country (sum :total)) (get :sum-total)))
    ("largest per country" "Invoice" ((group-sum :billing-country (max :total :as :largest)) (get :largest)))
    ("countries above 100" "Invoice"
     ((group-sum :billing-country (sum :total)) (where (> :sum-total 100)) (get :billing-country)))
    ("time per genre" "Track" ((group-sum :genre-id (sum :milliseconds)) (get :sum-milliseconds)))
    ("cheapest per genre" "Track" ((group-sum :genre-id (min :unit-price)) (get :min-unit-price)))
    ("per media type and genre" "Track" ((group-sum :media-type-id :genre-id) (get :count)))
    ("pieces sold" "InvoiceLine" ((summarize (sum :quantity)) (get :sum-quantity)))
    ("lines with a track" "InvoiceLine" ((summarize (count :track-id)) (get :count-track-id)))
    ("average price sold" "InvoiceLine" ((summarize (avg :unit-price)) (get :avg-unit-price)))
    ("albums of AC/DC" "Album" ((where (= :artist-id 1)) (sort-by :album-id) (get :title)))
    ("tracks with their album" "Track"
     ((join (table "Album") :on :album-id) (where (= :artist-id 22)) (sort-by :track-id) (select :track-id :title :name)))
    ("artists without an album" "Artist"
     ((join (table "Album") :on :artist-id :kind :left) (where (is-null :album-id)) (count)))
    ("invoices with customers" "Invoice"
     ((join (table "Customer") :on :customer-id) (where (= :country "Germany")) (sort-by :invoice-id) (select :invoice-id :last-name :total)))
    ("employees and their boss" "Employee"
     ((join (table "Employee") :on (:reports-to :employee-id) :kind :left) (sort-by :employee-id) (count)))
    ("playlists" "PlaylistTrack" ((group-sum :playlist-id) (get :count))))
  "The pipes: (NAME TABLE STAGES).  (table NAME) is another table of the same way.")

(defun qss-chinook-test--form (way table stages)
  "The code of the pipe TABLE STAGES in WAY."
  (let* ((stages (qss-spec-test--replace-tables
                  stages (lambda (name) (qss-chinook-test--source way name))))
         (pipe `(qss-> ,(qss-chinook-test--source way table) ,@stages)))
    (if (eq (car (car (last stages))) 'count) pipe `(qss-collect ,pipe))))

(defun qss-chinook-test--normal (v)
  "V in a form to compare: times as text (local wall clock)."
  (cond ((qss--time-p v) (format-time-string "%F %T" v))
        ((consp v) (cons (qss-chinook-test--normal (car v)) (qss-chinook-test--normal (cdr v))))
        (t v)))

(defun qss-chinook-test--same-p (a b)
  "Return non-nil if A and B are the same; numbers within a rounding error."
  (cond ((and (numberp a) (numberp b))
         (<= (abs (- a b)) (* 1e-9 (max 1 (abs a) (abs b)))))
        ((and (consp a) (consp b))
         (and (qss-chinook-test--same-p (car a) (car b))
              (qss-chinook-test--same-p (cdr a) (cdr b))))
        (t (equal a b))))

(defun qss-chinook-test--difference (want got)
  "The first difference of WANT and GOT as text, or nil if the same.
For lists of rows only the first row that differs, with its position."
  (cond ((qss-chinook-test--same-p want got) nil)
        ((and (proper-list-p want) (proper-list-p got) (not (eq (car got) 'error)))
         (or (cl-loop for x in want for y in got for i from 0
                      unless (qss-chinook-test--same-p x y)
                      return (format "row %d\n    memory %S\n    got    %S" i x y))
             (format "%d rows in memory, %d got" (length want) (length got))))
        (t (format "\n    memory %S\n    got    %S" want got))))

(defun qss-chinook-test--difference (want got)
  "The first difference of WANT and GOT as text, or nil if the same.
For lists of rows only the first row that differs, with its position."
  (cond ((qss-chinook-test--same-p want got) nil)
        ((and (proper-list-p want) (proper-list-p got) (not (eq (car got) 'error)))
         (or (cl-loop for x in want for y in got for i from 0
                      unless (qss-chinook-test--same-p x y)
                      return (format "row %d\n    memory %S\n    got    %S" i x y))
             (format "%d rows in memory, %d got" (length want) (length got))))
        (t (format "\n    memory %S\n    got    %S" want got))))

(defun qss-chinook-test--run (way)
  "Run every pipe in WAY; return the differences from memory as text."
  (mapconcat
   #'identity
   (delq nil
         (mapcar
          (lambda (c)
            (pcase-let* ((`(,name ,table ,stages) c)
                         (want (qss-chinook-test--normal
                                (eval (qss-chinook-test--form 'memory table stages) t)))
                         (got (condition-case err
                                  (qss-chinook-test--normal
                                   (eval (qss-chinook-test--form way table stages) t))
                                (error (list 'error (error-message-string err)))))
                         (diff (qss-chinook-test--difference want got)))
              (and diff (format "%s, %s: %s" way name diff))))
          qss-chinook-test--cases))
   "\n"))

;;; ------------------------------------------------------------------
;;; Tests

(ert-deftest qss-chinook-build ()
  "The SQLite file has all of Chinook."
  (let ((file (qss-chinook-test--file)))
    (should (equal (mapcar (lambda (tab) (qss-count (qss-sqlite file tab)))
                           '("Artist" "Album" "Track" "Invoice" "InvoiceLine"))
                   '(275 347 3503 412 2240)))
    (should (equal (qss-count (qss-sqlite file "Customer")) 59))))

(ert-deftest qss-chinook-statements ()
  "The script splits at ; outside quotes and comments."
  (should (equal (qss-chinook-test--statements
                  "/* a; b */ CREATE TABLE t (x);\r\nINSERT INTO t VALUES ('a;''b');\r\n-- c;\n")
                 '("/* a; b */ CREATE TABLE t (x)" "INSERT INTO t VALUES ('a;''b')"))))

(ert-deftest qss-chinook-memory ()
  "In memory, the pipes give what is known of Chinook."
  (should (equal (eval (qss-chinook-test--form 'memory "Track" '((count))) t) 3503))
  (should (equal (eval (qss-chinook-test--form 'memory "Album" '((where (= :artist-id 1)) (get :title))) t)
                 '("For Those About To Rock We Salute You" "Let There Be Rock")))
  (should (equal (eval (qss-chinook-test--form 'memory "Track" '((where (> :unit-price 1)) (count))) t)
                 213)))

(ert-deftest qss-chinook-every-way ()
  "Every pipe gives in each way what it gives in memory.
SQLite always; SQLite through ODBC if the driver is installed; the DSNs
of QSS_CHINOOK_DSNS."
  (let ((ways (qss-chinook-test--ways)))
    (message "qss-chinook: %d pipes, memory against %s" (length qss-chinook-test--cases)
             (mapconcat (lambda (w) (format "%s" w)) ways ", "))
    ;; every way, also after one that differs; then all differences at once
    (let ((report (mapconcat #'identity
                             (delq nil (mapcar (lambda (way)
                                                 (let ((r (qss-chinook-test--run way)))
                                                   (unless (string-empty-p r) r)))
                                               ways))
                             "\n")))
      (unless (string-empty-p report) (message "qss-chinook differences:\n%s" report))
      (should (equal "" report)))))

(provide 'qss-chinook-test)
;;; qss-chinook-test.el ends here
