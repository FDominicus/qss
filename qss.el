;;; qss.el --- Tables and one vocabulary for all collections  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus

;; Author: Friedrich Dominicus
;; Maintainer: Friedrich Dominicus
;; Version: 0.19.1
;; Package-Requires: ((emacs "29.1") (qss-contract "0.1.0"))
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
;;   qss: (processes) (group-sum user) (sort-by count :desc)
;;   qss: (sqlite (qss-demo-database) "Orders") (where (> :amount 100)) (first 20)
;;   qss: (files "~") (where (> :size "10mb")) (into sqlite "big-files")
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
;; file, `a' (or the right mouse button) offers the actions of the row
;; and words with the value at point, `w' copies the table as text, `g'
;; reads the source again.
;; On SQLite the words become SQL with `?' parameters until the rows
;; are needed.
;;
;; Only parts built into Emacs 29 are used: vtable, sqlite,
;; cl-generic and nadvice.
;;
;; The public interface is checked by contracts (qss-interface.el);
;; README.md says where the ideas come from.
;;
;; M-x qss opens a REPL where the tables appear right below the input.
;; It is a shell as well: !PROGRAM ARGS runs a program without a shell,
;; cd changes the directory.  A line that starts with | goes on with
;; the last table.
;;
;; The package has these parts, each in its own file:
;;   qss-table.el     tables, display, sources `files', `processes', `run'
;;   qss-pipe.el      the words and `qss->', `qss-eval'
;;   qss-sql.el       SQLite as a source (the words become SQL) and as
;;                    a target: (into sqlite "Name")
;;   qss-nu.el        Nushell as a source: (nu "ps | where cpu > 5"),
;;                    and as a word: (files) (nu "where size > 10kb")
;;   qss-nuon.el      reads and writes NUON, Nushell's format with types
;;   qss-present.el   presentations and accept (after CLIM): typed
;;                    objects, highlighted choices, typed commands
;;   qss-menu.el      actions at point: a, the right mouse button, w
;;   (contracts with blame, after Racket: the package qss-contract)
;;   qss-interface.el the public interface of qss with its contracts
;;   qss-sql-odbc.el  ODBC as a source: (odbc DSN TABLE), (into odbc DSN NAME);
;;                    needs the package qss-odbc, loaded only when used
;;   qss-edit.el      changing rows of SQLite tables: e d u, C-c C-c;
;;                    the words update-rows and delete-rows
;;   qss-repl.el      M-x qss, the REPL
;;   qss-examples.el  M-x qss-examples
;;   (cross references of all Emacs Lisp, in SQLite, with M-. and M-?:
;;    the package qss-elisp-xref)

;;; Code:

(require 'qss-table)
(require 'qss-pipe)
(require 'qss-sql)
(require 'qss-sql-odbc)                 ; qss-odbc itself only on first use
(require 'qss-present)
(require 'qss-menu)
(require 'qss-repl)
(require 'qss-nu)
(require 'qss-edit)
;; Last: the contracts of the interface, on everything above.
(require 'qss-interface)

;;; Tests

(declare-function ert-select-tests "ert" (selector universe))
(declare-function ert-test-name "ert" (test))

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
Meant for the command line with -f; the README shows the call.
Each test is named when it starts, not only when it is done (as ERT
does), so a test that hangs is the last line on the screen."
  (qss--load-tests)
  (qss--run-tests-batch))

(defun qss--run-tests-batch ()
  "Run the loaded tests named qss-... in batch mode, each named as it starts."
  (let* ((total (length (ert-select-tests "^qss-" t)))
         (n 0)
         (announce (lambda (test)
                     (setq n (1+ n))
                     (message "   start  %d/%d  %s" n total (ert-test-name test)))))
    (advice-add 'ert-run-test :before announce '((name . qss-announce)))
    (ert-run-tests-batch-and-exit "^qss-")))

;;; Tests of all packages: qss and the packages next to it

(defun qss--package-directories ()
  "The directory of qss and those of the qss packages next to it.
The packages lie side by side (~/.emacs.d/lib/qss/qss,
~/.emacs.d/lib/qss/qss-odbc ...); a directory counts if its name
starts with qss."
  (let* ((dir (directory-file-name (file-name-directory (locate-library "qss"))))
         (parent (file-name-directory dir)))
    (cons (file-name-as-directory dir)
          (mapcar #'file-name-as-directory
                  (seq-filter (lambda (d) (and (file-directory-p d)
                                               (not (file-equal-p d dir))))
                              (directory-files parent t "\\`qss"))))))

(defun qss--load-all-tests ()
  "Load the tests of qss and of the qss packages next to it.
Each package directory goes on the `load-path' first, so its tests find
their library also in an Emacs started with -Q.  Return a list of
\(PACKAGE . NUMBER-OF-TEST-FILES)."
  (require 'ert)
  (let ((found nil))
    (dolist (dir (qss--package-directories))
      (let ((files (directory-files dir t "\\`qss.*-test\\.el\\'")))
        (add-to-list 'load-path (directory-file-name dir) t)
        (when files
          (push (cons (file-name-nondirectory (directory-file-name dir)) (length files)) found))))
    ;; load after all directories are on the load-path: tests of one
    ;; package may need another (qss-elisp-xref needs qss-contract)
    (dolist (dir (qss--package-directories))
      (dolist (file (directory-files dir t "\\`qss.*-test\\.el\\'"))
        (load file nil t)))
    (nreverse found)))

;;;###autoload
(defun qss-run-all-tests ()
  "Run the tests of qss and of every qss package next to it.
The packages are the directories beside the one of qss whose names
start with qss: qss-contract, qss-odbc, qss-elisp-xref ..."
  (interactive)
  (let ((found (qss--load-all-tests)))
    (message "Tests of %s" (mapconcat (lambda (p) (format "%s (%d files)" (car p) (cdr p)))
                                      found ", ")))
  (save-window-excursion
    (ert-run-tests-interactively "^qss-"))
  (pop-to-buffer "*ert*"))

;;;###autoload
(defun qss-run-all-tests-batch ()
  "Run the tests of qss and of every qss package next to it, in batch mode.
As `qss-run-tests-batch', for all packages:

  emacs -Q --batch -L ~/.emacs.d/lib/qss/qss -l qss -f qss-run-all-tests-batch"
  (let ((found (qss--load-all-tests)))
    (message "Tests of %s" (mapconcat (lambda (p) (format "%s (%d files)" (car p) (cdr p)))
                                      found ", ")))
  (qss--run-tests-batch))

(provide 'qss)
;;; qss.el ends here
