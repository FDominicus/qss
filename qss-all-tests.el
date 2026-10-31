;;; qss-all-tests.el --- Run the tests of qss and all qss packages  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; One call for the tests of qss and of the qss packages next to it
;; (qss-contract, qss-odbc, qss-elisp-xref ... in the same parent
;; directory), from a shell with only the directory of qss given:
;;
;;   emacs -Q --batch -L ~/.emacs.d/lib/qss/qss -l qss-all-tests -f qss-run-all-tests-batch
;;
;; Loading this file puts the packages on the `load-path' before qss
;; is loaded, since qss needs qss-contract.  In a running Emacs,
;; M-x qss-run-all-tests does the same.

;;; Code:

(let* ((dir (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
       (parent (file-name-directory dir)))
  ;; a package directory holds the file of its name: qss-odbc/qss-odbc.el
  (dolist (d (directory-files parent t "\\`qss"))
    (when (and (file-directory-p d)
               (file-exists-p (expand-file-name (concat (file-name-nondirectory d) ".el") d)))
      (add-to-list 'load-path d t))))

(require 'qss)

(provide 'qss-all-tests)
;;; qss-all-tests.el ends here
