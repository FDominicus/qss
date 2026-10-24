;;; qss-manual-test.el --- The worked example of the manual still works  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; doc/qss-manual-example.el is shown in the manual; these tests make
;; sure that what the manual shows works.

;;; Code:

(require 'ert)
(require 'qss)

(let ((dir (expand-file-name "doc" (file-name-directory
                                    (or load-file-name (locate-library "qss"))))))
  (when (file-exists-p (expand-file-name "qss-manual-example.el" dir))
    (load (expand-file-name "qss-manual-example.el" dir) nil t)))

(declare-function my-libraries "qss-manual-example")
(declare-function my-library-name "qss-manual-example")
(declare-function my-library-requires "qss-manual-example")

(defmacro qss-mantest--needs-example (&rest body)
  "Run BODY if the example of the manual is loaded."
  (declare (indent 0))
  `(progn (skip-unless (featurep 'qss-manual-example)) ,@body))

(ert-deftest qss-manual-example-source ()
  (qss-mantest--needs-example
    ;; The function, not the short name `libraries': a compiled test
    ;; expands qss-> before the example registers the short name.
    (let ((tab (qss-> (my-libraries "\\`subr") (sort-by size :desc))))
      (should (qss-table-p tab))
      (should (member "subr" (qss-> tab (get name) collect)))
      (should (eq 'size (plist-get (cdr (assq 'size (qss-table-columns tab))) :type)))
      (should (equal (mapcar #'car (qss-table-columns tab)) '(name size path))))))

(ert-deftest qss-manual-example-is-a-file ()
  (qss-mantest--needs-example
    (let ((row (car (qss-table-rows (my-libraries "\\`subr\\'")))))
      (should (eq (qss-presentation-type row) 'library))
      (should (qss-presentation-subtype-p 'library 'file))
      (should (file-exists-p (qss-file-name row)))
      ;; the commands on files apply, and its own
      (let ((labels (mapcar #'car (qss-commands-for row 'library))))
        (should (member "Compare with file..." labels))
        (should (member "Show what it requires" labels)))
      ;; typed text: a library by its name
      (should (equal (my-library-name (qss-presentation-parse 'library "subr")) "subr")))))

(ert-deftest qss-manual-example-command ()
  (qss-mantest--needs-example
    (save-window-excursion
      (let ((buf (my-library-requires (car (qss-table-rows (my-libraries "\\`qss-sql\\'"))))))
        (unwind-protect
            (with-current-buffer buf
              (should (member "qss-pipe" (mapcar (lambda (r) (plist-get r :feature))
                                                 (qss-table-rows qss--table)))))
          (kill-buffer buf))))))

(ert-deftest qss-manual-example-contract ()
  (qss-mantest--needs-example
    (let ((qss-contract-enabled t))
      (should (string-match-p "my-libraries: argument 1 (regexp) must satisfy stringp"
                              (condition-case e (progn (my-libraries 42) "")
                                (qss-contract-violation (cadr e))))))))

(ert-deftest qss-manual-example-into ()
  (qss-mantest--needs-example
    (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
    (let* ((dir (make-temp-file "qss-manual-" t))
           (qss-database (expand-file-name "c.sqlite3" dir)))
      (unwind-protect
          (let ((rel (qss-> (my-libraries "\\`qss-") (into sqlite "libs"))))
            (should (equal (mapcar (lambda (c) (nth 3 c)) (qss-relation-columns rel)) '(nil size nil)))
            ;; the stored row has its path: it is a file again
            (should (eq 'file (qss-presentation-type (car (qss-collect rel))))))
        (delete-directory dir t)))))

(provide 'qss-manual-test)
;;; qss-manual-test.el ends here
