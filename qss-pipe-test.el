;;; qss-pipe-test.el --- Tests for qss-pipe  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-pipe-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss-pipe)

(defvar qss-contract-enabled)           ; qss-contract.el; dynamic in the tests

(defmacro qss-ptest--with-dir (var &rest body)
  "Bind VAR to a directory with files of known size, then run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "qss-pipe-" t)))
     (unwind-protect
         (progn
           (dolist (f '(("a.el" . 100) ("b.el" . 3000) ("c.txt" . 50)
                        ("d.org" . 20000) ("E.EL" . 10)))
             (with-temp-file (expand-file-name (car f) ,var)
               (insert (make-string (cdr f) ?x))))
           (make-directory (expand-file-name "sub" ,var))
           ,@body)
       (delete-directory ,var t))))

(defun qss-ptest--names (table)
  (mapcar (lambda (r) (qss-field r 'name)) (qss-table-rows table)))

(defconst qss-ptest--people
  (list '(:name "Anna" :age 40 :city "Bruchsal")
        '(:name "Bernd" :age 25 :city "Karlsruhe")
        '(:name "Clara" :age nil :city "Bruchsal")
        '(:name "dieter" :age 31 :city nil)))

;;; Comparisons

(ert-deftest qss-op-semantics ()
  (should (qss-op-= "a" "a"))
  (should-not (qss-op-= "a" "b"))
  (should (qss-op-= 3 3.0))
  (should-not (qss-op-= nil nil))                ; NULL = NULL is not true
  (should-not (qss-op-/= nil 1))
  (should (qss-op-< "Anna" "Bernd"))
  (should (qss-op-<= 2 2))
  (should-not (qss-op-> nil 1))
  (should (qss-op-in "b" '("a" "b")))
  (should-not (qss-op-in nil '(nil)))
  (should (qss-op-like "Bruchsal" "bruch%"))
  (should (qss-op-like "a.b" "a._"))
  (should-not (qss-op-like "axb" "a.b"))          ; a dot is no wildcard
  (should-not (qss-op-like nil "%"))
  (should-error (qss-op-< 1 "a") :type 'user-error))

(ert-deftest qss-op-times ()
  (let ((t1 (qss--to-time "2026-10-20"))
        (t2 (qss--to-time "2026-10-20 07:30")))
    (should (qss-op-< t1 t2))
    (should (qss-op-> t2 "2026-10-20"))
    (should (qss-op-< "2026-10-19" t1))
    (should-error (qss--to-time "gestern") :type 'user-error)))

;;; where

(ert-deftest qss-where-data ()
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (should (equal (qss-ptest--names (qss-> tab (where (> :age 30))))
                   '("Anna" "dieter")))
    (should (equal (qss-ptest--names (qss-> tab (where (= :city "Bruchsal"))))
                   '("Anna" "Clara")))
    (should (equal (qss-ptest--names (qss-> tab (where (is-null :age)))) '("Clara")))
    (should (equal (qss-ptest--names (qss-> tab (where (like :name "%A%")))) '("Anna" "Clara")))
    (should (equal (qss-ptest--names (qss-> tab (where (in :city '("Karlsruhe" "Mannheim")))))
                   '("Bernd")))
    (should (equal (qss-ptest--names
                    (qss-> tab (where (or (< :age 30) (and (not-null :city) (not (= :city "Bruchsal")))))))
                   '("Bernd")))
    ;; NULL: neither > nor <= matches Clara
    (should (= 3 (+ (qss-count (qss-> tab (where (> :age 30))))
                    (qss-count (qss-> tab (where (<= :age 30)))))))))

(ert-deftest qss-where-three-valued ()
  "NULL as in SQL: (not (= :city X)) does not keep rows without a city."
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (should (equal (qss-ptest--names (qss-> tab (where (not (= :city "Bruchsal"))))) '("Bernd")))
    (should (equal (qss-ptest--names (qss-> tab (where (or (> :age 30) (is-null :city)))))
                   '("Anna" "dieter")))
    (should (equal (qss-ptest--names (qss-> tab (where (not (and (> :age 30) (= :city "Bruchsal"))))))
                   '("Bernd")))                   ; Clara: age NULL -> unknown
    (should (equal (qss-ptest--names (qss-> tab (where (or (> :age 100) (= :city "Bruchsal")))))
                   '("Anna" "Clara")))))

(ert-deftest qss-filter-is-where ()
  "The word filter and the function qss-filter are where."
  (should (eq (qss--word-function 'filter) 'qss-filter))
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (should (equal (qss-ptest--names (qss-> tab (filter (> :age 30))))
                   (qss-ptest--names (qss-> tab (where (> :age 30))))))
    (should (equal (qss-ptest--names (qss-filter tab (lambda (r) (let ((age (plist-get r :age)))
                                                                    (and age (> age 30))))))
                   (qss-ptest--names (qss-> tab (where (> :age 30))))))))

(ert-deftest qss-where-lisp-variables ()
  (let ((tab (qss-make-table "people" qss-ptest--people))
        (limit 30)
        (cities '("Karlsruhe")))
    (should (equal (qss-ptest--names (qss-> tab (where (> :age limit)))) '("Anna" "dieter")))
    (should (equal (qss-ptest--names (qss-> tab (where (in :city cities)))) '("Bernd")))))

(ert-deftest qss-where-lisp-with-it ()
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    ;; with it: ordinary Lisp, keywords stay keywords
    (should (equal (qss-ptest--names
                    (qss-> tab (where (string-prefix-p "d" (plist-get it :name)))))
                   '("dieter")))
    (should (equal (qss-ptest--names (qss-where tab (lambda (r) (equal (qss-field r 'age) 25))))
                   '("Bernd")))))

(ert-deftest qss-where-files ()
  (qss-ptest--with-dir dir
    (let ((tab (qss-> (files dir) (where (like :name "%.el")))))
      (should (equal (sort (qss-ptest--names tab) #'string<) '("E.EL" "a.el" "b.el")))
      (should (cl-every #'qss-file-p (qss-table-rows tab)))       ; files stay files
      (should (string-match-p "| where (like :name \"%.el\")\\'" (qss-table-title tab))))
    (should (equal (qss-ptest--names (qss-> (files dir) (where (= :type "dir")))) '("sub")))
    (should (equal (qss-ptest--names (qss-> (files dir) (where (> :modified "2000-01-01"))
                                            (where (= :name "c.txt"))))
                   '("c.txt")))))

;;; sort-by, first, last

(ert-deftest qss-sort-by-values ()
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (should (equal (qss-ptest--names (qss-> tab (sort-by age))) '("Bernd" "dieter" "Anna" "Clara")))
    (should (equal (qss-ptest--names (qss-> tab (sort-by age :desc))) '("Anna" "dieter" "Bernd" "Clara")))
    (should (equal (qss-ptest--names (qss-> tab (sort-by name desc))) '("dieter" "Clara" "Bernd" "Anna")))
    (should (equal (qss-ptest--names (qss-> tab (sort-by :city))) '("Anna" "Clara" "Bernd" "dieter")))
    (should-error (qss-sort-by tab 'age :sideways) :type 'user-error)))

(ert-deftest qss-sort-files-by-size ()
  (qss-ptest--with-dir dir
    (should (equal (qss-ptest--names (qss-> (files dir) (where (= :type "file"))
                                            (sort-by size :desc) (first 3)))
                   '("d.org" "b.el" "a.el")))
    (should (equal (qss-ptest--names (qss-> (files dir) (where (= :type "file"))
                                            (sort-by size) (last 2)))
                   '("b.el" "d.org")))))

(ert-deftest qss-first-last-defaults ()
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (should (equal (qss-ptest--names (qss-> tab first)) '("Anna")))
    (should (equal (qss-ptest--names (qss-> tab (last))) '("dieter")))
    (should (= 4 (qss-count (qss-> tab (first 10)))))))

;;; select, reject, get, columns, count

(ert-deftest qss-select-keeps-objects ()
  (qss-ptest--with-dir dir
    (let ((tab (qss-> (files dir) (select name size))))
      (should (equal (mapcar #'car (qss-table-columns tab)) '(name size)))
      (should (eq 'size (plist-get (cdr (assq 'size (qss-table-columns tab))) :type)))
      (should (qss-file-p (car (qss-table-rows tab))))
      (should (qss-field (car (qss-table-rows tab)) 'path)))       ; a field outside the selection
    (should (equal (mapcar #'car (qss-table-columns (qss-> (files dir) (reject modified mode user))))
                   '(name type size)))))

(ert-deftest qss-column-specs-do-not-grow ()
  "Each stage keeps the column specs as they are, without repeating keys."
  (qss-ptest--with-dir dir
    (let* ((tab (qss-> (files dir) (where (like :name "%.el")) (sort-by size :desc)
                       (first 5) (select name size modified)))
           (spec (assq 'size (qss-table-columns tab))))
      (should (equal spec '(size :type size :align right :format qss-format-size))))))

(ert-deftest qss-plain-values-are-rows ()
  "A list of numbers or strings is a table with the column value."
  (should (equal (qss-> '(3 1 2) (sort-by value) collect) '(1 2 3)))
  (should (equal (qss-> '(3 1 2) (where (> :value 1)) collect) '(3 2)))
  (should (equal (mapcar #'car (qss-table-columns (qss-as-table '("a" "b")))) '(value))))

(ert-deftest qss-select-projects-data ()
  "Property lists are cut down, objects are not."
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (should (equal (qss-table-rows (qss-> tab (select city name) (first 2)))
                   '((:city "Bruchsal" :name "Anna") (:city "Karlsruhe" :name "Bernd"))))
    (should (equal (car (qss-table-rows (qss-> tab (reject age))))
                   '(:name "Anna" :city "Bruchsal")))
    (should (equal (mapcar #'car (qss-table-columns (qss-> tab (reject age)))) '(name city)))
    (should-error (qss-> tab (select no-such-column)) :type 'user-error)))

(ert-deftest qss-collect-rows ()
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (should (equal (qss-> tab (where (> :age 30)) (select name) collect)
                   '((:name "Anna") (:name "dieter"))))
    (should (equal (qss-> tab (where (> :age 100)) collect) nil))
    ;; after get: the plain values
    (should (equal (qss-> tab (get name) collect) '("Anna" "Bernd" "Clara" "dieter")))))

(ert-deftest qss-get-columns-count ()
  (let ((tab (qss-make-table "people" qss-ptest--people)))
    (let ((ages (qss-> tab (get age))))
      (should (equal (mapcar (lambda (r) (qss-field r 'age)) (qss-table-rows ages)) '(40 25 nil 31)))
      (should (equal (mapcar #'car (qss-table-columns ages)) '(age))))
    (should (equal (mapcar (lambda (r) (qss-field r 'column)) (qss-table-rows (qss-> tab columns)))
                   '(name age city)))
    (should (= 2 (qss-> tab (where (= :city "Bruchsal")) count)))))

;;; each, group-by

(ert-deftest qss-each-builds-rows ()
  (let* ((tab (qss-make-table "people" qss-ptest--people))
         (out (qss-> tab (where (not-null :age))
                     (each (list :name (upcase (plist-get it :name))
                                 :next (1+ (plist-get it :age)))))))
    (should (equal (mapcar #'car (qss-table-columns out)) '(name next)))
    (should (equal (qss-field (car (qss-table-rows out)) 'next) 41))
    (let ((scalars (qss-> tab (each (length (plist-get it :name))))))
      (should (equal (mapcar (lambda (r) (qss-field r 'value)) (qss-table-rows scalars))
                     '(4 5 5 6))))))

(ert-deftest qss-group-by-counts ()
  (let* ((tab (qss-make-table "people" qss-ptest--people))
         (g (qss-> tab (group-by city) (sort-by count :desc))))
    (should (equal (mapcar #'car (qss-table-columns g)) '(city count)))
    (should (equal (mapcar (lambda (r) (list (qss-field r 'city) (qss-field r 'count)))
                           (qss-table-rows g))
                   '(("Bruchsal" 2) ("Karlsruhe" 1) (nil 1))))
    ;; RET on a group: its rows as a table
    (with-current-buffer (qss-default-action (car (qss-table-rows g)))
      (unwind-protect
          (progn
            (should (equal (qss-ptest--names qss--table) '("Anna" "Clara")))
            (should (equal (mapcar #'car (qss-table-columns qss--table)) '(name age city))))
        (kill-buffer)))))

(ert-deftest qss-group-files-by-type ()
  (qss-ptest--with-dir dir
    (let ((g (qss-> (files dir) (group-by type) (sort-by type))))
      (should (equal (mapcar (lambda (r) (list (qss-field r 'type) (qss-field r 'count)))
                             (qss-table-rows g))
                     '(("dir" 1) ("file" 5)))))))

;;; Reading again

(ert-deftest qss-derived-table-refreshes ()
  (qss-ptest--with-dir dir
    (let ((tab (qss-> (files dir) (where (like :name "%.el")) (sort-by size :desc))))
      (should (= 3 (qss-count tab)))
      (with-temp-file (expand-file-name "z.el" dir) (insert (make-string 99999 ?x)))
      (with-temp-file (expand-file-name "z.txt" dir) (insert "x"))
      (let ((again (funcall (qss-table-source tab))))
        (should (equal (qss-ptest--names again) '("z.el" "b.el" "a.el" "E.EL")))
        (should (equal (qss-table-title again) (qss-table-title tab))))
      ;; g in the buffer
      (with-current-buffer (qss-show tab)
        (unwind-protect
            (progn
              (delete-file (expand-file-name "z.el" dir))
              (qss-refresh)
              (should (= 3 (qss-count qss--table)))
              (should-not (string-match-p "z\\.el" (buffer-string))))
          (kill-buffer))))))

(ert-deftest qss-list-without-source-has-no-refresh ()
  (let ((tab (qss-> qss-ptest--people (where (> :age 30)))))
    (should (= 2 (qss-count tab)))
    (should (null (qss-table-source tab)))))

;;; Macro and command

(ert-deftest qss-unknown-word ()
  (should-error (macroexpand-all '(qss-> (files "/") (wehre (> :size 1))))))

(ert-deftest qss-eval-command ()
  (qss-ptest--with-dir dir
    (let ((n (qss-eval (format "(files %S) (where (= :type \"file\")) count" dir))))
      (should (= n 5)))
    (let ((tab (qss-eval (format "(files %S) (where (= :type \"file\")) (sort-by size :desc) (first 2) (select name size)" dir))))
      (unwind-protect
          (progn
            (should (qss-table-p tab))
            (should (equal (qss-ptest--names tab) '("d.org" "b.el")))
            (with-current-buffer (get-buffer (format "*qss %s*" (qss-table-title tab)))
              (should (string-match-p "d\\.org" (buffer-string)))
              (should (string-match-p "20k" (buffer-string)))))
        (kill-buffer (format "*qss %s*" (qss-table-title tab)))))))

;;; Tables as text

(ert-deftest qss-to-formats ()
  "text, org and md show the cells as the table does; csv and tsv the plain values."
  (let ((tab (qss-make-table "t" (list (list :name "Ada, L." :size 2048 :note "a|b")
                                       (list :name "Grace" :size 10 :note nil))
                             '(name (size :type size) note))))
    (should (equal (qss-to tab 'text)
                   "name     size  note\nAda, L.    2k  a|b\nGrace      10\n"))
    (should (equal (qss-to tab 'org)
                   (concat "| name    | size | note      |\n"
                           "|---------+------+-----------|\n"
                           "| Ada, L. |   2k | a\\vert{}b |\n"
                           "| Grace   |   10 |           |\n")))
    (should (equal (qss-to tab 'md)
                   (concat "| name    | size | note |\n"
                           "|---------|-----:|------|\n"
                           "| Ada, L. |   2k | a\\|b |\n"
                           "| Grace   |   10 |      |\n")))
    (should (equal (qss-to tab 'csv) "name,size,note\n\"Ada, L.\",2048,a|b\nGrace,10,\n"))
    (should (equal (qss-to tab "tsv") "name\tsize\tnote\nAda, L.\t2048\ta|b\nGrace\t10\t\n"))
    ;; the contract stops an unknown format at the boundary ...
    (when (and (featurep 'qss-interface) qss-contract-enabled)
      (should-error (qss-to tab 'xml) :type 'qss-contract-violation))
    ;; ... and without contracts the function itself refuses it
    (let ((qss-contract-enabled nil))
      (should-error (qss-to tab 'xml) :type 'user-error))))

(ert-deftest qss-to-csv-quoting-and-times ()
  (let* ((time (encode-time '(5 11 7 22 10 2026 nil -1 nil)))
         (tab (qss-make-table "t" (list (list :text "say \"hi\"\nnow" :when time))
                              '(text (when :type time)))))
    (should (equal (qss-to tab 'csv) "text,when\n\"say \"\"hi\"\"\nnow\",2026-10-22 07:11:05\n"))
    (should (equal (qss-to tab 'text) "text          when\nsay \"hi\" now  2026-10-22 07:11\n"))))

(ert-deftest qss-to-as-word ()
  (qss-ptest--with-dir dir
    (let ((csv (qss-> (files dir) (where (= :name "b.el")) (select name size) (to csv))))
      (should (string-match-p "\\`name,size\nb\\.el,[0-9]+\n\\'" csv)))))

(provide 'qss-pipe-test)
;;; qss-pipe-test.el ends here
