;;; qss.el --- Tables and one vocabulary for all collections  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus

;; Author: Friedrich Dominicus
;; Maintainer: Friedrich Dominicus
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: data, tools, convenience
;; URL: https://github.com/FDominicus/qss
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; qss brings the idea of Nushell to Emacs: data as tables, and one
;; small vocabulary that works on all of them, whether the rows are
;; files, processes, property lists or rows of an SQLite table.
;;
;;   M-x qss-eval
;;   qss: (files "~") (where (> :size 100000)) (sort-by size :desc) (first 10)
;;   qss: (processes) (group-by user) (sort-by count :desc)
;;   qss: (sqlite (qss-demo-database) "Orders") (where (> :amount 100)) (first 20)
;;
;; In Lisp the same is a macro:
;;
;;   (qss-> (sqlite (qss-demo-database) "Orders")
;;          (where (and (> :amount 100) (in :customer-no '(1 3))))
;;          (sort-by order-date :desc)
;;          (select order-no customer-no amount)
;;          collect)
;;
;; `qss-demo-database' creates a small SQLite database to try things
;; with; M-x qss-examples shows many more examples.
;;
;; Rows stay the objects they are: in the table buffer RET opens a
;; file, `a' offers the actions of the row, `g' reads the source again.
;; On SQLite the words become SQL with `?' parameters until the rows
;; are needed.
;;
;; Only parts built into Emacs 29 are used: vtable, sqlite and
;; cl-generic.
;;
;; M-x qss opens a REPL where the tables appear right below the input.
;; It is a shell as well: !PROGRAM ARGS runs a program without a shell,
;; cd changes the directory.
;;
;; The package has these parts, each in its own file:
;;   qss-table.el     tables, display, sources `files', `processes', `run'
;;   qss-pipe.el      the words and `qss->', `qss-eval'
;;   qss-sql.el       SQLite as a source; the words become SQL
;;   qss-nu.el        Nushell as a source: (nu "ps | where cpu > 5")
;;   qss-repl.el      M-x qss, the REPL
;;   qss-examples.el  M-x qss-examples

;;; Code:

(require 'qss-table)
(require 'qss-pipe)
(require 'qss-sql)
(require 'qss-repl)
(require 'qss-nu)

;;; Tests

(defun qss--load-tests ()
  "Load the test files of qss from the directory of the package."
  (require 'ert)
  (let* ((dir (file-name-directory (locate-library "qss")))
         (files (directory-files dir t "\\`qss-.*-test\\.el\\'")))
    (unless files (user-error "No test files in %s" dir))
    (dolist (file files) (load file nil t))))

;;;###autoload
(defun qss-run-tests ()
  "Run the qss test suite in this Emacs and show the results.
Some of them open and close table buffers; the window layout is
restored afterwards."
  (interactive)
  (qss--load-tests)
  (save-window-excursion
    (ert-run-tests-interactively "^qss-"))
  (pop-to-buffer "*ert*"))

;;;###autoload
(defun qss-run-tests-batch ()
  "Run the qss test suite in batch mode and exit with its result.
Meant for the command line with -f; the README shows the call."
  (qss--load-tests)
  (ert-run-tests-batch-and-exit "^qss-"))

(provide 'qss)
;;; qss.el ends here
