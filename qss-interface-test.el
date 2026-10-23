;;; qss-interface-test.el --- Tests for the contracts of qss  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-interface-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defun qss-itest--violation (fn)
  "Return the message of the contract violation FN signals, or nil."
  (let ((qss-contract-enabled t))
    (condition-case err (progn (funcall fn) nil)
      (qss-contract-violation (cadr err)))))

(ert-deftest qss-interface-every-contract-is-on-a-function ()
  "A contract on a misspelt name would never be checked."
  (should (> (length (qss-contract-functions)) 40))
  (dolist (fn (qss-contract-functions))
    (should (fboundp fn))))

(ert-deftest qss-interface-blames-own-methods ()
  "A method of an own row type that returns the wrong thing is named.
The methods stay defined: they only apply to the test type."
  (cl-defstruct qss-itest-thing name)
  (cl-defmethod qss-columns ((_ qss-itest-thing)) '(name (size :type megabytes)))
  (let ((m (qss-itest--violation
            (lambda () (qss-make-table "t" (list (make-qss-itest-thing :name "x")))))))
    (should (string-match-p "\\`qss-columns: the result must satisfy (listof qss-column-spec)" m))
    (should (string-match-p "blame: qss-columns itself, the method for qss-itest-thing" m)))
  (cl-defmethod qss-actions ((_ qss-itest-thing)) (list (cons 'open #'ignore)))
  (should (string-match-p "element 0 is (open \\. ignore), not qss-action"
                          (qss-itest--violation (lambda () (qss-actions (make-qss-itest-thing))))))
  (cl-defmethod qss-presentation-type ((_ qss-itest-thing)) 'gadget)
  (should (string-match-p "qss-presentation-type: the result must satisfy qss-presentation-type-name, got gadget"
                          (qss-itest--violation (lambda () (qss-presentation-type (make-qss-itest-thing))))))
  (cl-defmethod qss-stored-fields ((_ qss-itest-thing)) '("path"))
  (should (string-match-p "blame: qss-stored-fields itself, the method for qss-itest-thing"
                          (qss-itest--violation (lambda () (qss-stored-fields (make-qss-itest-thing)))))))

(ert-deftest qss-interface-blames-the-caller ()
  (should (string-match-p "\\`qss-first: argument 2 must satisfy natnump, got \"x\""
                          (qss-itest--violation (lambda () (qss-> '((:a 1)) (first "x"))))))
  (should (string-match-p "qss-sqlite: argument 1 (file) must satisfy stringp"
                          (qss-itest--violation (lambda () (qss-sqlite 42)))))
  (should (string-match-p "qss-accept: argument 1 (type) must satisfy qss-presentation-type-name"
                          (qss-itest--violation (lambda () (qss-accept 'no-such-type)))))
  ;; nil for an optional argument means "not given"
  (should (qss-table-p (qss-files nil))))

(ert-deftest qss-interface-caller-named ()
  "A contract broken inside checked code names the checked function it came from."
  (let ((m (qss-itest--violation (lambda () (qss-continue '((:a 1)) '(first "x"))))))
    (should (string-match-p "\\`qss-first: argument 2 must satisfy natnump" m))
    (should (string-match-p "blame: the caller, qss-continue" m))))

(ert-deftest qss-interface-contracts-source ()
  (let ((tab (qss-> (contracts) (where (= :function "qss-sqlite")))))
    (should (= 1 (qss-count tab)))
    (should (equal (qss-field (car (qss-table-rows tab)) 'contract)
                   "(-> stringp (opt (or/c stringp symbolp)) qss-relation-p)")))
  (should (equal (qss-> (contracts) (where (= :function "qss-columns")) (get kind) collect)
                 '("generic"))))

(ert-deftest qss-interface-help-shows-contract ()
  (save-window-excursion
    (describe-function 'qss-sort-by)
    (with-current-buffer (help-buffer)
      (should (string-search "Contract: (-> qss-tabular qss-column-name (opt symbolp) qss-result)"
                             (buffer-string))))))

(ert-deftest qss-interface-repl-shows-violation ()
  "In the REPL a broken contract is an error message like any other."
  (let ((qss-last nil) (qss-contract-enabled t))
    (when (get-buffer "*qss*") (kill-buffer "*qss*"))
    (save-window-excursion
      (with-current-buffer (qss)
        (unwind-protect
            (progn
              (goto-char (point-max))
              (insert "(processes) (first -1)")
              (qss-repl-send)
              (should (string-search "\nContract violation: qss-first: argument 2 must satisfy natnump, got -1\n"
                                     (buffer-string))))
          (kill-buffer "*qss*"))))))

(provide 'qss-interface-test)
;;; qss-interface-test.el ends here
