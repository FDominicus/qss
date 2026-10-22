;;; qss-nu-test.el --- Tests for Nushell as a source  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The conversion tests run everywhere; the tests that call Nushell are
;; skipped when `qss-nu-program' is not installed.
;;
;;   emacs --batch -L . -l qss-nu-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defun qss-ntest--parse (json)
  "Parse JSON as `qss-nu' does."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defmacro qss-ntest--with-nu (&rest body)
  "Run BODY if Nushell is installed, otherwise skip."
  (declare (indent 0))
  `(progn (skip-unless (executable-find qss-nu-program)) ,@body))

;;; Without Nushell

(ert-deftest qss-nu-convert-records ()
  (let ((rows (qss-nu--convert
               (qss-ntest--parse "[{\"name\":\"a\",\"Mount Point\":\"/\",\"CamelCase\":1,\"nested\":{\"x\":2}}]"))))
    (should (equal rows '((:name "a" :mount-point "/" :camel-case 1 :nested (:x 2)))))))

(ert-deftest qss-nu-columns-union-and-types ()
  (let ((cols (qss-nu--columns '((:name "a" :size 10) (:name "b" :size 20 :extra "x")))))
    (should (equal cols '(name (size :type size) extra)))))

(ert-deftest qss-nu-missing-program ()
  (let ((qss-nu-program "qss-no-such-nushell"))
    (should-error (qss-nu "ls") :type 'user-error)))

(ert-deftest qss-nu-datetimes-and-sizes ()
  "Datetimes become Lisp times; size columns are shown as sizes."
  (let* ((rows (qss-nu--convert
                (qss-ntest--parse "[{\"name\":\"a\",\"modified\":\"2026-10-22T10:54:54.842186897+02:00\",\"mem\":380461056,\"note\":\"2026-10-22\"}]")))
         (cols (qss-nu--columns rows)))
    (should (qss--time-p (plist-get (car rows) :modified)))
    (should (equal (format-time-string "%Y-%m-%d %H:%M" (plist-get (car rows) :modified))
                   (format-time-string "%Y-%m-%d %H:%M" (encode-time (iso8601-parse "2026-10-22T10:54:54+02:00")))))
    (should (equal (plist-get (car rows) :note) "2026-10-22"))       ; a plain date stays text
    (should (equal (assq 'modified cols) '(modified :type time)))
    (should (equal (assq 'mem cols) '(mem :type size)))))

;;; With Nushell

(ert-deftest qss-nu-list-of-records ()
  (qss-ntest--with-nu
    (let ((tab (qss-nu "[{a: 1, b: 'x'} {a: 2, b: 'y'} {a: 3, b: 'z'}]")))
      (should (equal (mapcar #'car (qss-table-columns tab)) '(a b)))
      (should (equal (qss-> tab (where (> :a 1)) (get b) collect) '("y" "z")))
      (should (equal (qss-table-title tab) "nu [{a: 1, b: 'x'} {a: 2, b: 'y'} {a: 3, b: 'z'}]")))
    ;; as a source in qss->
    (should (equal (qss-> (nu "[{a: 1} {a: 5}]") (where (> :a 2)) collect) '((:a 5))))))

(ert-deftest qss-nu-record-and-values ()
  (qss-ntest--with-nu
    (should (equal (qss-collect (qss-nu "{x: 1, 'Some Key': 'v'}"))
                   '((:column x :value 1) (:column some-key :value "v"))))
    (should (equal (qss-collect (qss-nu "[3 1 2] | sort")) '(1 2 3)))
    ;; a time in a single record is shown readably
    (should (string-match-p "\\`2026-10-22 [0-9][0-9]:[0-9][0-9]\\'"
                            (plist-get (car (qss-collect (qss-nu "{t: ('2026-10-22T10:00:00+02:00' | into datetime)}")))
                                       :value)))
    (should (equal (qss-nu "40 + 2") 42))
    (should (equal (qss-nu "'text'") "text"))
    (should (equal (qss-collect (qss-nu "[]")) nil))))

(ert-deftest qss-nu-error ()
  (qss-ntest--with-nu
    (let ((err (should-error (qss-nu "this-is-not-a-command-qss") :type 'user-error)))
      (should (string-match-p "Nushell" (cadr err))))))

(ert-deftest qss-nu-ls-and-ret ()
  (qss-ntest--with-nu
    (let ((dir (make-temp-file "qss-nu-" t)))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "big.txt" dir) (insert (make-string 5000 ?x)))
            (with-temp-file (expand-file-name "small.txt" dir) (insert "x"))
            (make-directory (expand-file-name "sub" dir))
            (let* ((default-directory (file-name-as-directory dir))
                   (tab (qss-nu "ls")))
              (should (equal (sort (qss-> tab (get name) collect) #'string<)
                             '("big.txt" "small.txt" "sub")))
              (should (eq 'size (plist-get (cdr (assq 'size (qss-table-columns tab))) :type)))
              (should (eq 'time (plist-get (cdr (assq 'modified (qss-table-columns tab))) :type)))
              (should (= 3 (qss-> tab (where (> :modified "2000-01-01")) count)))
              (should (equal (qss-> tab (where (= :type "file")) (sort-by size :desc) (first 1) (get name) collect)
                             '("big.txt")))
              ;; refresh runs in the same directory
              (with-temp-file (expand-file-name "new.txt" dir))
              (let ((default-directory "/"))
                (should (member "new.txt" (qss-> (funcall (qss-table-source tab)) (get name) collect)))))
            ;; RET on a file row opens the file, also from elsewhere
            (let* ((default-directory (file-name-as-directory dir))
                   (row (car (qss-> (nu "ls") (where (= :name "small.txt")) collect))))
              (let ((default-directory "/"))
                (save-window-excursion
                  (qss-default-action row)
                  (should (equal (buffer-name) "small.txt"))
                  (kill-buffer)))))
        (delete-directory dir t)))))

(ert-deftest qss-nu-in-repl ()
  (qss-ntest--with-nu
    (let ((qss-last nil))
      (when (get-buffer "*qss*") (kill-buffer "*qss*"))
      (save-window-excursion
        (with-current-buffer (qss)
          (unwind-protect
              (progn
                (goto-char (point-max))
                (insert "(nu \"[{n: 1} {n: 2} {n: 3}]\") (where (> :n 1))")
                (let ((start (point)))
                  (qss-repl-send)
                  (should (string-match-p "^2 rows" (buffer-substring-no-properties start (point-max))))))
            (kill-buffer "*qss*")))))))

(provide 'qss-nu-test)
;;; qss-nu-test.el ends here
