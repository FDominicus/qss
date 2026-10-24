;;; qss-sql-test.el --- Tests for qss-sql  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-sql-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss-sql)

(defvar qss-stest--file nil)

(defun qss-stest--make-db ()
  "Create a small database and return its file.
The names are German on purpose (CamelCase, umlauts, NULL), to test
the mapping of names to keys."
  (let ((file (make-temp-file "qss-sql-" nil ".sqlite3")))
    (qss-sql--with-db db file
      (sqlite-execute db "CREATE TABLE \"HT_Kunden\" (\"KundeNr\" INTEGER PRIMARY KEY,
        \"Name1\" TEXT, \"Ort\" TEXT, \"Umsatz\" REAL, \"Straße\" TEXT, \"Datum\" TEXT,
        \"Betrag\" DECIMAL_TEXT, \"PLZ\" INTEGER)")
      (dolist (row '((1 "Anna Meier" "Bruchsal" 1200.5 "Hauptstr. 1" "2024-10-31 00:00:00" "1094.80" 76646)
                     (2 "Bernd" "Karlsruhe" 300.0 nil "2025-01-15 00:00:00" "20.00" 76131)
                     (3 "clara" "Bruchsal" nil "Am Markt 3" "2026-10-20 00:00:00" nil 76646)
                     (4 "Dieter_X" nil 50.0 "Weg 4" nil "5.50" nil)
                     (5 "Eva" "Heidelsheim" 999.99 "Kirchstr. 5" "2026-10-21 12:00:00" "100.00" 76646)))
        (sqlite-execute db "INSERT INTO \"HT_Kunden\" VALUES (?,?,?,?,?,?,?,?)" (vconcat row)))
      (sqlite-execute db "CREATE TABLE \"HT_WiegedatenExtern\" (\"WiegescheinNr\" INTEGER,
        \"EWWiegeart\" TEXT, \"Nettogewicht\" REAL)")
      (sqlite-execute db "INSERT INTO \"HT_WiegedatenExtern\" VALUES (900544, 'E', 10.0)"))
    file))

(defmacro qss-stest--with-db (&rest body)
  (declare (indent 0))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let ((qss-stest--file (qss-stest--make-db)))
       (unwind-protect (progn ,@body)
         (delete-file qss-stest--file)))))

(defun qss-stest--k () (qss-sqlite qss-stest--file "HT_Kunden"))

(defun qss-stest--ids (x)
  (mapcar (lambda (r) (qss-field r 'kunde-nr)) (qss-table-rows (qss-as-table x))))

;;; Names and schema

(ert-deftest qss-sql-names ()
  (should (eq (qss-sql-name-key "KundeNr") 'kunde-nr))
  (should (eq (qss-sql-name-key "Straße") 'strasse))
  (should (eq (qss-sql-name-key "HT_WiegedatenExtern") 'ht-wiegedaten-extern))
  (should (eq (qss-sql-name-key "EWWiegeart") 'ew-wiegeart))
  (should (eq (qss-sql-name-key "PLZ") 'plz))
  (should (eq (qss-sql-name-key "Name1") 'name1))
  (should (eq (qss-sql-name-key "länderschlüssel") 'laenderschluessel))
  (should (eq (qss-sql-name-key "Q_Abfrage 3") 'q-abfrage-3)))

(ert-deftest qss-sql-schema-and-tables ()
  (qss-stest--with-db
    (let ((tabs (qss-tables qss-stest--file)))
      (should (equal (mapcar (lambda (r) (qss-field r 'name)) (qss-table-rows tabs))
                     '("HT_Kunden" "HT_WiegedatenExtern")))
      (should (equal (mapcar (lambda (r) (qss-field r 'key)) (qss-table-rows tabs))
                     '(ht-kunden ht-wiegedaten-extern))))
    ;; a table by database name, key or keyword
    (should (equal (qss-relation-table (qss-sqlite qss-stest--file 'ht-kunden)) "HT_Kunden"))
    (should (equal (qss-relation-table (qss-sqlite qss-stest--file :ht-wiegedaten-extern))
                   "HT_WiegedatenExtern"))
    (should (equal (qss-relation-table (qss-sqlite qss-stest--file "ht_kunden")) "HT_Kunden"))
    (should-error (qss-sqlite qss-stest--file "NoSuchTable") :type 'user-error)
    (let ((cols (qss-as-table (qss-columns-of (qss-stest--k)))))
      (should (equal (mapcar (lambda (r) (qss-field r 'column)) (qss-table-rows cols))
                     '(kunde-nr name1 ort umsatz strasse datum betrag plz)))
      (should (equal (qss-field (nth 4 (qss-table-rows cols)) 'name) "Straße")))))

(ert-deftest qss-sql-name-collision ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((file (make-temp-file "qss-coll-" nil ".sqlite3")))
    (unwind-protect
        (progn
          (qss-sql--with-db db file
            (sqlite-execute db "CREATE TABLE t (\"Masse\" REAL, \"Maße\" REAL)"))
          (should-error (qss-sql-schema file)))
      (delete-file file))))

;;; SQL text

(ert-deftest qss-sql-text-and-params ()
  (qss-stest--with-db
    (let* ((limit 500)
           (text (qss-sql (qss-> (sqlite qss-stest--file "HT_Kunden")
                                 (where (and (> :umsatz limit) (like :name1 "A%")))
                                 (sort-by umsatz :desc)
                                 (first 3)))))
      (should (string-match-p "FROM \"HT_Kunden\"" text))
      (should (string-search "\"Umsatz\" > ?" text))
      (should (string-search "\"Name1\" LIKE ?" text))
      (should (string-search "ORDER BY \"Umsatz\" IS NULL, \"Umsatz\" DESC" text))
      (should (string-search "LIMIT ?" text))
      (should (string-search "Parameters: (500 \"A%\" 3)" text))
      ;; values never become text in the SQL
      (should-not (string-match-p "500\\|A%" (car (split-string text "\nParameters")))))
    (should (string-match-p "\"Straße\" IS NULL"
                            (qss-sql (qss-> (sqlite qss-stest--file "HT_Kunden")
                                            (where (is-null :strasse))))))
    (should-error (qss-> (sqlite qss-stest--file "HT_Kunden") (where (> :umsatzz 1)))
                  :type 'user-error)
    (should-error (qss-> (sqlite qss-stest--file "HT_Kunden") (sort-by no-such-column))
                  :type 'user-error)))

;;; The same condition: SQL and memory return the same rows

(defmacro qss-stest--same (&rest stages)
  "Run STAGES as a query and on the fetched rows; both must be equal."
  `(let ((sql (qss-> (qss-stest--k) ,@stages))
         (mem (qss-> (qss-as-table (qss-stest--k)) ,@stages)))
     (should (qss-relation-p sql))
     (should (qss-table-p mem))
     (should (equal (qss-stest--ids sql) (qss-stest--ids mem)))
     (qss-stest--ids sql)))

(ert-deftest qss-sql-same-as-memory ()
  (qss-stest--with-db
    (let ((limit 300) (orte '("Bruchsal" "Heidelsheim")) (seit (qss--to-time "2026-01-01")))
      (should (equal (qss-stest--same (where (> :umsatz limit))) '(1 5)))
      (should (equal (qss-stest--same (where (>= :umsatz limit))) '(1 2 5)))
      (should (equal (qss-stest--same (where (= :ort "Bruchsal"))) '(1 3)))
      (should (equal (qss-stest--same (where (/= :ort "Bruchsal"))) '(2 5)))   ; NULL not included
      (should (equal (qss-stest--same (where (in :ort orte))) '(1 3 5)))
      (should (equal (qss-stest--same (where (in :ort '()))) '()))
      (should (equal (qss-stest--same (where (like :name1 "%a%"))) '(1 3 5)))  ; ignoring case
      (should (equal (qss-stest--same (where (like :name1 "Dieter_X"))) '(4)))
      (should (equal (qss-stest--same (where (is-null :umsatz))) '(3)))
      (should (equal (qss-stest--same (where (not-null :strasse))) '(1 3 4 5)))
      (should (equal (qss-stest--same (where (or (is-null :ort) (< :umsatz 400)))) '(2 4)))
      (should (equal (qss-stest--same (where (not (= :ort "Bruchsal")))) '(2 5)))
      (should (equal (qss-stest--same (where (> :datum "2025-06-01"))) '(3 5)))
      (should (equal (qss-stest--same (where (> :datum seit))) '(3 5)))      ; Lisp time as a parameter
      (should (equal (qss-stest--same (where (= :plz 76646)) (where (> :kunde-nr 1))) '(3 5)))
      (should (equal (qss-stest--same (where (> :umsatz (* 2 limit)))) '(1 5))))))

(ert-deftest qss-sql-sort-first-same-as-memory ()
  (qss-stest--with-db
    (should (equal (qss-stest--same (sort-by umsatz)) '(4 2 5 1 3)))       ; NULL last
    (should (equal (qss-stest--same (sort-by umsatz :desc)) '(1 5 2 4 3))) ; descending too
    (should (equal (qss-stest--same (sort-by ort) (sort-by plz :desc)) '(1 3 5 2 4)))
    (should (equal (qss-stest--same (first 2)) '(1 2)))
    (should (equal (qss-stest--same (first 3) (first 2)) '(1 2)))
    ;; where after first: a subquery, not just a WHERE in front
    (should (equal (qss-stest--same (first 2) (where (= :ort "Bruchsal"))) '(1)))
    (should (equal (qss-stest--same (sort-by umsatz :desc) (first 3) (sort-by name1))
                   '(1 2 5)))
    (should (string-match-p "FROM (\n" (qss-sql (qss-> (qss-stest--k) (first 2)
                                                      (where (= :ort "Bruchsal"))))))))

;;; What cannot become SQL

(ert-deftest qss-sql-falls-back-to-memory ()
  (qss-stest--with-db
    ;; Lisp with it
    (let ((r (qss-> (sqlite qss-stest--file "HT_Kunden")
                    (where (string-prefix-p "B" (plist-get it :name1))))))
      (should (qss-table-p r))
      (should (equal (qss-stest--ids r) '(2))))
    ;; a function over a column
    (let ((r (qss-> (sqlite qss-stest--file "HT_Kunden")
                    (where (string-suffix-p "X" :name1)))))
      (should (qss-table-p r))
      (should (equal (qss-stest--ids r) '(4))))
    ;; then it continues in memory, with SQL before
    (let ((r (qss-> (sqlite qss-stest--file "HT_Kunden")
                    (where (= :ort "Bruchsal"))
                    (each (list :nr (plist-get it :kunde-nr) :gross (upcase (plist-get it :name1)))))))
      (should (equal (mapcar (lambda (x) (qss-field x 'gross)) (qss-table-rows r))
                     '("ANNA MEIER" "CLARA"))))
    (should (equal (mapcar (lambda (r) (qss-field r 'plz))
                           (qss-table-rows (qss-> (qss-stest--k) (where (= :ort "Bruchsal")) (get plz))))
                   '(76646 76646)))
    ;; group-by is SQL now (GROUP BY); groups sorted by key, NULL last,
    ;; so equal counts keep that order
    (let ((g (qss-> (qss-stest--k) (group-by ort) (sort-by count :desc))))
      (should (qss-relation-p g))
      (should (equal (mapcar (lambda (r) (list (qss-field r 'ort) (qss-field r 'count)))
                             (qss-collect g))
                     '(("Bruchsal" 2) ("Heidelsheim" 1) ("Karlsruhe" 1) (nil 1)))))
    (should (equal (qss-stest--ids (qss-> (qss-stest--k) (sort-by kunde-nr) (last 2))) '(4 5)))
    (should-error (qss-> (qss-stest--k) (where (string-suffix-p "X" :name1)) sql)
                  :type 'user-error)))

;;; count, select, display

(ert-deftest qss-sql-count ()
  (qss-stest--with-db
    (should (= 5 (qss-> (sqlite qss-stest--file "HT_Kunden") count)))
    (should (= 2 (qss-> (qss-stest--k) (where (= :ort "Bruchsal")) count)))
    (should (= 3 (qss-> (qss-stest--k) (first 3) count)))
    (should (= 1 (qss-> (qss-stest--k) (first 2) (where (= :ort "Bruchsal")) count)))))

(ert-deftest qss-sql-select-projects ()
  "Select cuts down, in the SQL and in the rows; what is cut is gone."
  (qss-stest--with-db
    (let* ((rel (qss-> (qss-stest--k) (select name1 umsatz) (where (> :umsatz 500))))
           (tab (qss-as-table rel)))
      (should (string-search "SELECT \"Name1\", \"Umsatz\"\nFROM" (qss-sql rel)))
      (should (equal (mapcar #'car (qss-table-columns tab)) '(name1 umsatz)))
      (should (eq 'number (plist-get (cdr (assq 'umsatz (qss-table-columns tab))) :type)))
      (should (equal (qss-table-rows tab)
                     '((:name1 "Anna Meier" :umsatz 1200.5) (:name1 "Eva" :umsatz 999.99)))))
    ;; order as given
    (should (equal (car (qss-table-rows (qss-as-table (qss-> (qss-stest--k) (select umsatz kunde-nr)))))
                   '(:umsatz 1200.5 :kunde-nr 1)))
    ;; Betrag is text (DECIMAL_TEXT), not a number
    (should (equal (plist-get (car (qss-table-rows (qss-as-table (qss-> (qss-stest--k) (select betrag)))))
                              :betrag)
                   "1094.80"))
    (let ((tab (qss-as-table (qss-> (qss-stest--k) (reject datum betrag plz strasse)))))
      (should (equal (mapcar #'car (qss-table-columns tab)) '(kunde-nr name1 ort umsatz)))
      (should (equal (car (qss-table-rows tab))
                     '(:kunde-nr 1 :name1 "Anna Meier" :ort "Bruchsal" :umsatz 1200.5))))
    ;; after select the other columns are gone, through a subquery too
    (should-error (qss-> (qss-stest--k) (select name1) (where (> :umsatz 1))) :type 'user-error)
    (should-error (qss-> (qss-stest--k) (select name1) (first 2) (sort-by umsatz)) :type 'user-error)
    (should (equal (qss-table-rows (qss-as-table (qss-> (qss-stest--k) (select kunde-nr name1)
                                                        (first 3) (where (like :name1 "%a%")))))
                   '((:kunde-nr 1 :name1 "Anna Meier") (:kunde-nr 3 :name1 "clara"))))
    ;; cut down, then continue in memory
    (should (equal (qss-table-rows (qss-as-table (qss-> (qss-stest--k) (select kunde-nr ort)
                                                        (where (string-prefix-p "B" (or (plist-get it :ort) "")))
                                                        (select kunde-nr))))
                   '((:kunde-nr 1) (:kunde-nr 3))))
    (should (= 5 (qss-> (qss-stest--k) (select name1) count)))
    ;; sort first, then select: ORDER BY may name a column that is not selected
    (let ((rel (qss-> (qss-stest--k) (sort-by umsatz :desc) (select kunde-nr name1))))
      (should (string-search "SELECT \"KundeNr\", \"Name1\"" (qss-sql rel)))
      (should (string-search "ORDER BY \"Umsatz\" IS NULL, \"Umsatz\" DESC" (qss-sql rel)))
      (should (equal (mapcar (lambda (r) (plist-get r :kunde-nr)) (qss-collect rel)) '(1 5 2 4 3))))
    (should (equal (qss-> (qss-stest--k) (where (> :umsatz 100)) (sort-by umsatz) (select name1)
                          (get name1) collect)
                   '("Bernd" "Eva" "Anna Meier")))
    ;; and in memory the same
    (should (equal (qss-> (qss-as-table (qss-stest--k)) (sort-by umsatz :desc) (select kunde-nr) (get kunde-nr) collect)
                   '(1 5 2 4 3)))
    (should (equal (qss-> (qss-stest--k) (where (> :umsatz 500)) (select kunde-nr umsatz) collect)
                   '((:kunde-nr 1 :umsatz 1200.5) (:kunde-nr 5 :umsatz 999.99))))
    (should-error (qss-> (qss-stest--k) (select no-such-column)) :type 'user-error)))

(ert-deftest qss-sql-display-limit-and-refresh ()
  (qss-stest--with-db
    (let ((qss-sql-display-limit 2))
      (with-current-buffer (qss-display (qss-> (qss-stest--k) (sort-by kunde-nr)))
        (unwind-protect
            (progn
              (should (string-match-p "\\[first 2 of 5\\]" (qss-table-title qss--table)))
              (should (= 2 (length (qss-table-rows qss--table))))
              (should (string-match-p "Anna Meier" (buffer-string)))
              (should-not (string-match-p "clara" (buffer-string))))
          (kill-buffer))))
    (with-current-buffer (qss-display (qss-> (qss-stest--k) (where (= :ort "Bruchsal"))))
      (unwind-protect
          (progn
            (should (= 2 (length (qss-table-rows qss--table))))
            (qss-sql--with-db db qss-stest--file
              (sqlite-execute db "INSERT INTO \"HT_Kunden\" (\"KundeNr\", \"Name1\", \"Ort\")
                                  VALUES (6, 'Neu', 'Bruchsal')"))
            (qss-refresh)
            (should (= 3 (length (qss-table-rows qss--table))))
            (should (string-match-p "Neu" (buffer-string))))
        (kill-buffer)))))

(ert-deftest qss-sql-tables-ret-and-eval ()
  (qss-stest--with-db
    (let ((tabs (qss-tables qss-stest--file)))
      (with-current-buffer (qss-default-action (cadr (qss-table-rows tabs)))
        (unwind-protect
            (progn
              (should (string-match-p "HT_WiegedatenExtern" (qss-table-title qss--table)))
              (should (string-match-p "900544" (buffer-string))))
          (kill-buffer))))
    (let ((text (qss-eval (format "(sqlite %S \"HT_Kunden\") (where (> :umsatz 100)) sql"
                                  qss-stest--file))))
      (should (stringp text))
      (should (string-search "WHERE (\"Umsatz\" > ?)" text)))
    (let ((tab-buffer (qss-eval (format "(sqlite %S \"HT_Kunden\") (where (> :umsatz 100)) (select name1 umsatz)"
                                        qss-stest--file))))
      (ignore tab-buffer)
      (let ((buf (cl-find-if (lambda (b) (string-match-p "select name1 umsatz" (buffer-name b)))
                             (buffer-list))))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "Eva" (buffer-string)))
          (kill-buffer))))))

(ert-deftest qss-sql-demo-database ()
  "The demo database exists after the first call and has both tables."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((temporary-file-directory (make-temp-file "qss-demo-" t)))
    (unwind-protect
        (let ((file (qss-demo-database)))
          (should (file-exists-p file))
          (should (equal (qss-demo-database) file))          ; created once
          (should (equal (mapcar (lambda (r) (qss-field r 'name))
                                 (qss-collect (qss-tables file)))
                         '("Customers" "Orders")))
          (should (= 6 (qss-> (sqlite (qss-demo-database) "Orders") count)))
          ;; the example of the README
          (should (equal (qss-> (sqlite (qss-demo-database) "Orders")
                                (where (> :amount 100))
                                (select order-no amount)
                                collect)
                         '((:order-no 4711 :amount 120.0) (:order-no 4713 :amount 300.0)
                           (:order-no 4715 :amount 999.99) (:order-no 4716 :amount 150.0)))))
      (delete-directory temporary-file-directory t))))

;;; Sizes and durations as text

(ert-deftest qss-sql-quantities ()
  "\"10kb\" means 10000 bytes in SQL as in memory, but not in a text column."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((file (make-temp-file "qss-sql-q-" nil ".sqlite3")))
    (unwind-protect
        (progn
          (qss-sql--with-db db file
            (sqlite-execute db "CREATE TABLE \"Files\" (\"Name\" TEXT, \"Size\" INTEGER,
               \"Took\" INTEGER, \"Note\" TEXT, \"Raw\")")
            (dolist (row '(("a" 500 2000000000 "10kb" 20000)
                           ("b" 20000 7200000000000 "x" 5)
                           ("c" nil nil nil nil)))
              (sqlite-execute db "INSERT INTO \"Files\" VALUES (?,?,?,?,?)" (vconcat row))))
          (cl-flet ((names (rel) (mapcar (lambda (r) (plist-get r :name)) (qss-collect rel))))
            (let ((rel (qss-> (sqlite file "Files") (where (> :size "10kb")))))
              (should (string-search "Parameters: (10000)" (qss-sql rel)))
              (should (equal (names rel) '("b"))))
            (should (equal (names (qss-> (sqlite file "Files") (where (< :took "1hr")))) '("a")))
            ;; text column: "10kb" stays text
            (should (equal (names (qss-> (sqlite file "Files") (where (= :note "10kb")))) '("a")))
            ;; a column without type: ordered comparisons convert
            (should (equal (names (qss-> (sqlite file "Files") (where (> :raw "10kb")))) '("a")))
            (should (equal (names (qss-> (sqlite file "Files") (where (in :size '("500b" "20kb")))))
                           '("a" "b")))
            ;; the same rows as in memory
            (should (equal (names (qss-> (sqlite file "Files") (where (>= :size "0.5kb"))))
                           (names (qss-> (qss-as-table (qss-sqlite file "Files"))
                                         (where (>= :size "0.5kb"))))))))
      (delete-file file))))

(ert-deftest qss-sql-affinity ()
  (should (eq (qss-sql--affinity "INTEGER") 'numeric))
  (should (eq (qss-sql--affinity "decimal(10,2)") 'numeric))
  (should (eq (qss-sql--affinity "DECIMAL_TEXT") 'text))
  (should (eq (qss-sql--affinity "VARCHAR(20)") 'text))
  (should (eq (qss-sql--affinity "") 'none))
  (should (eq (qss-sql--affinity "BLOB") 'none)))

;;; Writing: into sqlite

(defmacro qss-stest--with-central (dir &rest body)
  "Run BODY with DIR a fresh directory and `qss-database' a file in it.
The real central database is never touched."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let* ((,dir (make-temp-file "qss-into-" t))
            (qss-database (expand-file-name "central.sqlite3" ,dir)))
       (unwind-protect (progn ,@body)
         (delete-directory ,dir t)))))

(ert-deftest qss-into-sqlite-round-trip ()
  "Files go in with their types and path and come back as a query."
  (qss-stest--with-central dir
    (with-temp-file (expand-file-name "big.txt" dir) (insert (make-string 12000 ?x)))
    (with-temp-file (expand-file-name "small.txt" dir) (insert "x"))
    (let ((rel (qss-> (files dir) (where (like :name "%.txt")) (into sqlite "Dateien"))))
      (should (qss-relation-p rel))
      (should (file-exists-p qss-database))
      (should (= 2 (qss-count rel)))
      (should (equal (mapcar (lambda (c) (list (car c) (nth 2 c) (nth 3 c))) (qss-relation-columns rel))
                     '((name "TEXT" nil) (type "TEXT" nil) (size "INTEGER" size)
                       (modified "TEXT" time) (mode "TEXT" nil) (user "TEXT" nil)
                       (path "TEXT" nil))))
      ;; sizes are shown as sizes again, times as ISO text
      (let ((text (qss-to rel 'text)))
        (should (string-match-p "^big\\.txt +file +12k +[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9:]\\{8\\} " text)))
      ;; the words work on the stored table, in SQL
      (let ((big (qss-> (sqlite "Dateien") (where (> :size "10kb")))))
        (should (string-search "Parameters: (10000)" (qss-sql big)))
        (should (equal (qss-> big (get name) collect) '("big.txt"))))
      (should (= 2 (qss-> (sqlite "Dateien") (where (> :modified "2020-01-01")) count)))
      ;; the path is stored, RET opens the file.  Paths are compared as
      ;; files, not as text: on Windows one file has several spellings
      ;; (c: or C:, short 8.3 names from TEMP).
      (let ((row (car (qss-> (sqlite "Dateien") (where (= :name "small.txt")) collect)))
            (small (expand-file-name "small.txt" dir))
            (before (buffer-list)))
        (should (file-equal-p (plist-get row :path) small))
        (unwind-protect
            (save-window-excursion
              (qss-default-action row)
              (should (cl-some (lambda (b) (and (buffer-file-name b)
                                                (file-equal-p (buffer-file-name b) small)))
                               (buffer-list))))
          ;; whatever happens, close what the test opened
          (dolist (b (buffer-list))
            (unless (memq b before) (kill-buffer b))))))))

(ert-deftest qss-into-sqlite-exists-replace-append ()
  (qss-stest--with-central dir
    (let ((rows '((:n 1 :text "a") (:n 2 :text "b"))))
      (qss-> rows (into sqlite "T"))
      ;; neither :replace nor :append: refused, nothing changed
      (should-error (qss-> rows (into sqlite "t")) :type 'user-error)
      (should (= 2 (qss-count (qss-sqlite "T"))))
      (should (= 4 (qss-count (qss-> rows (into sqlite "T" :append)))))
      (should (= 1 (qss-count (qss-> '((:n 9 :text "z")) (into sqlite "T" :replace t)))))
      (should (equal (qss-> (sqlite "T") collect) '((:n 9 :text "z"))))
      ;; the table of qss types is not shown
      (should (equal (qss-> (tables) (get name) collect) '("T")))
      (should-error (qss-> rows (into sqlite "T" :update)) :type 'user-error)
      (should-error (qss-> rows (into csv "x")) :type 'user-error))))

(ert-deftest qss-into-sqlite-values ()
  "Integers, reals, times, t, symbols and lists are stored sensibly."
  (qss-stest--with-central dir
    (let* ((time (encode-time '(5 11 7 22 10 2026 nil -1 nil)))
           (file (expand-file-name "other.db" dir))
           (rel (qss-> (list (list :i 1 :r 1.5 :when time :flag t :kind 'dir :list '(1 "a") :empty nil)
                             (list :i 2 :r 2 :when nil :flag nil :kind nil :list nil :empty nil))
                       (into sqlite "v" :file file))))
      (should-not (file-exists-p qss-database))
      (should (equal (mapcar (lambda (c) (nth 2 c)) (qss-relation-columns rel))
                     '("INTEGER" "REAL" "TEXT" "INTEGER" "TEXT" "TEXT" "TEXT")))
      (should (equal (car (qss-collect rel))
                     '(:i 1 :r 1.5 :when "2026-10-22 07:11:05" :flag 1 :kind "dir"
                          :list "(1 a)" :empty nil))))))

(ert-deftest qss-into-sqlite-from-query ()
  "A query can be stored in another file and goes on as a query there."
  (qss-stest--with-central dir
    (let ((temporary-file-directory dir))
      (let ((rel (qss-> (sqlite (qss-demo-database) "Orders") (where (is-null :status))
                        (into sqlite "open"))))
        (should (equal (qss-relation-table rel) "open"))
        (should (equal (qss-> rel (sort-by amount :desc) (get order-no) collect) '(4715 4713)))))))

(ert-deftest qss-sqlite-one-argument ()
  (qss-stest--with-central dir
    (should-error (qss-sqlite "Nothing") :type 'user-error) ; no central file yet
    (qss-> '((:a 1)) (into sqlite "one"))
    (should (= 1 (qss-count (qss-sqlite "one"))))
    (should-error (qss-sqlite (expand-file-name "x.sqlite3" dir)) :type 'user-error)))

(ert-deftest qss-database-follows-data-directory ()
  "Without a file of its own the central database lies in `qss-data-directory'."
  (let ((user-emacs-directory "/home/u/.emacs.d/"))
    (let ((qss-data-directory nil) (qss-database nil))
      (should (equal (qss-database-file) (expand-file-name "/home/u/.emacs.d/qss.sqlite3"))))
    (let ((qss-data-directory "/local/qss/") (qss-database nil))
      (should (equal (qss-database-file) (expand-file-name "/local/qss/qss.sqlite3")))
      (should (equal (qss-data-file "x.db") (expand-file-name "/local/qss/x.db"))))
    ;; a file set of its own wins
    (let ((qss-data-directory "/local/qss/") (qss-database "/other/central.sqlite3"))
      (should (equal (qss-database-file) (expand-file-name "/other/central.sqlite3"))))))

(ert-deftest qss-into-sqlite-makes-data-directory ()
  "Storing into the central file makes `qss-data-directory' if need be."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((dir (make-temp-file "qss-data-" t))
         (qss-data-directory (expand-file-name "not/yet/" dir))
         (qss-database nil))
    (unwind-protect
        (progn
          (qss-> '((:a 1)) (into sqlite "one"))
          (should (file-exists-p (expand-file-name "qss.sqlite3" qss-data-directory)))
          (should (= 1 (qss-count (qss-sqlite "one")))))
      (delete-directory dir t))))

(provide 'qss-sql-test)
;;; qss-sql-test.el ends here
