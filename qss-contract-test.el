;;; qss-contract-test.el --- Tests for qss-contract  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-contract-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qss-contract)

;; Defined by the tests themselves.
(declare-function qss-ctest-f nil)
(declare-function qss-ctest-g nil)
(declare-function qss-ctest-h nil)
(declare-function qss-ctest-r nil)
(declare-function qss-ctest-re nil)
(declare-function qss-ctest-late nil)
(declare-function qss-ctest-n nil)
(declare-function qss-ctest-o nil)
(declare-function qss-ctest-docgen nil)
(declare-function qss-ctest-docfun nil)

(defmacro qss-ctest--with-fns (fns &rest body)
  "Run BODY, then remove the contracts and definitions of FNS."
  (declare (indent 1))
  `(unwind-protect (let ((qss-contract-enabled t)) ,@body)
     (dolist (f ',fns)
       (qss-contract-remove f)
       (fmakunbound f))))

(defun qss-ctest--message (form-fn)
  "Return the message of the contract violation FORM-FN signals."
  (condition-case err (progn (funcall form-fn) nil)
    (qss-contract-violation (cadr err))))

(ert-deftest qss-contract-flat-and-combinators ()
  (cl-flet ((ok (spec v) (null (funcall (qss-contract--compile spec) v))))
    (should (ok 'stringp "a"))
    (should-not (ok 'stringp 1))
    (should (ok 'any nil))
    (should (ok '(or/c null stringp) nil))
    (should-not (ok '(or/c null stringp) 3))
    (should (ok '(and/c integerp (lambda (x) (> x 0))) 3))
    (should-not (ok '(and/c integerp (lambda (x) (> x 0))) -3))
    (should (ok '(not/c null) 1))
    (should (ok '(one-of a b "c") "c"))
    (should-not (ok '(one-of a b) 'c))
    (should (ok '(listof symbolp) '(a b)))
    (should-not (ok '(listof symbolp) '(a 1)))
    (should-not (ok '(listof symbolp) '(a . b)))
    (should (ok '(list/c symbolp integerp) '(a 1)))
    (should-not (ok '(list/c symbolp integerp) '(a 1 2)))
    (should (ok '(cons/c stringp functionp) (cons "x" #'car)))
    (should (ok '(-> any any) #'car))
    (should-not (ok '(-> any any) 3))
    (should-error (qss-contract--compile 'no-such-predicate-at-all))
    (should-error (qss-contract--compile '(frobnicate x)))))

(ert-deftest qss-contract-details ()
  (should (equal (funcall (qss-contract--compile '(listof symbolp)) '(a b 3))
                 "element 2 is 3, not symbolp"))
  (should (equal (funcall (qss-contract--compile '(cons/c stringp any)) '(1 . 2))
                 "its car is not stringp")))

(ert-deftest qss-contract-blames-the-caller ()
  (qss-ctest--with-fns (qss-ctest-f qss-ctest-g)
    (defun qss-ctest-f (name &optional count) (format "%s %s" name count))
    (qss-define-contract qss-ctest-f (-> stringp (opt integerp) stringp))
    (should (equal (qss-ctest-f "a" 2) "a 2"))
    (should (equal (qss-ctest-f "a" nil) "a nil"))     ; nil is "not given"
    (let ((m (qss-ctest--message (lambda () (qss-ctest-f 1)))))
      (should (string-match-p "\\`qss-ctest-f: argument 1 (name) must satisfy stringp, got 1" m))
      (should (string-match-p "blame: the caller (outside the checked functions)" m))
      (should (string-match-p "contract: (-> stringp (opt integerp) stringp)" m)))
    (let ((m (qss-ctest--message (lambda () (qss-ctest-f "a" "two")))))
      (should (string-match-p "argument 2 (count) must satisfy integerp, got \"two\"" m)))
    ;; called from another checked function: that one is named
    (defun qss-ctest-g (x) (qss-ctest-f x))
    (qss-define-contract qss-ctest-g (-> any stringp))
    (should (string-match-p "blame: the caller, qss-ctest-g"
                            (qss-ctest--message (lambda () (qss-ctest-g 42)))))))

(ert-deftest qss-contract-blames-the-function ()
  (qss-ctest--with-fns (qss-ctest-h)
    (defun qss-ctest-h (x) (if (> x 0) "fine" 'oops))
    (qss-define-contract qss-ctest-h (-> integerp stringp))
    (should (equal (qss-ctest-h 1) "fine"))
    (let ((m (qss-ctest--message (lambda () (qss-ctest-h -1)))))
      (should (string-match-p "\\`qss-ctest-h: the result must satisfy stringp, got oops" m))
      (should (string-match-p "blame: qss-ctest-h itself" m)))))

(ert-deftest qss-contract-rest-arguments ()
  (qss-ctest--with-fns (qss-ctest-r)
    (defun qss-ctest-r (a &rest more) (cons a more))
    (qss-define-contract qss-ctest-r (-> symbolp (rest stringp) list))
    (should (equal (qss-ctest-r 'x "a" "b") '(x "a" "b")))
    (should (string-match-p "argument 3 must satisfy stringp, got 7"
                            (qss-ctest--message (lambda () (qss-ctest-r 'x "a" 7)))))))

(ert-deftest qss-contract-generic-and-later-methods ()
  "A contract on a generic function holds for every method, also later ones."
  (qss-ctest--with-fns (qss-ctest-gen)
    (cl-defgeneric qss-ctest-gen (x) "Return a list of symbols for X.")
    (cl-defmethod qss-ctest-gen ((_x integer)) '(a b))
    (qss-define-contract qss-ctest-gen (-> any (listof symbolp)))
    (should (equal (qss-ctest-gen 1) '(a b)))
    ;; a method added afterwards, which breaks the contract
    (cl-defmethod qss-ctest-gen ((_x string)) '(a "b"))
    (let ((m (qss-ctest--message (lambda () (qss-ctest-gen "s")))))
      (should (string-match-p "the result must satisfy (listof symbolp), got (a \"b\") (element 1 is \"b\", not symbolp)" m))
      (should (string-match-p "blame: qss-ctest-gen itself, the method for string" m)))))

(ert-deftest qss-contract-survives-redefinition ()
  (qss-ctest--with-fns (qss-ctest-re)
    (defun qss-ctest-re (x) x)
    (qss-define-contract qss-ctest-re (-> integerp integerp))
    (defun qss-ctest-re (x) (* 2 x))
    (should (= 4 (qss-ctest-re 2)))
    (should-error (qss-ctest-re "2") :type 'qss-contract-violation)))

(ert-deftest qss-contract-defined-before-the-function ()
  (qss-ctest--with-fns (qss-ctest-late)
    (qss-define-contract qss-ctest-late (-> integerp integerp))
    (defun qss-ctest-late (x) x)
    (should-error (qss-ctest-late "x") :type 'qss-contract-violation)))

(ert-deftest qss-contract-named-types ()
  (qss-ctest--with-fns (qss-ctest-n)
    (qss-define-contract-type 'qss-ctest-pair '(list/c symbolp integerp) "A symbol and a number.")
    (defun qss-ctest-n (pairs) (length pairs))
    (qss-define-contract qss-ctest-n (-> (listof qss-ctest-pair) natnump))
    (should (= 2 (qss-ctest-n '((a 1) (b 2)))))
    (should (string-match-p "element 1 is (b), not qss-ctest-pair"
                            (qss-ctest--message (lambda () (qss-ctest-n '((a 1) (b)))))))
    ;; a named contract is looked up when checking: it can change
    (qss-define-contract-type 'qss-ctest-pair '(list/c symbolp any))
    (should (= 1 (qss-ctest-n '((b "x")))))
    (put 'qss-ctest-pair 'qss-contract-type nil)))

(ert-deftest qss-contract-off-and-remove ()
  (qss-ctest--with-fns (qss-ctest-o)
    (defun qss-ctest-o (x) x)
    (qss-define-contract qss-ctest-o (-> integerp integerp))
    (let ((qss-contract-enabled nil))
      (should (equal (qss-ctest-o "x") "x")))
    (should-error (qss-ctest-o "x") :type 'qss-contract-violation)
    (should (equal (qss-contract-of 'qss-ctest-o) '(-> integerp integerp)))
    (should (memq 'qss-ctest-o (qss-contract-functions)))
    ;; a new contract replaces the old one
    (qss-define-contract qss-ctest-o (-> stringp stringp))
    (should (equal (qss-ctest-o "x") "x"))
    (should (= 1 (length (let (l) (advice-mapc (lambda (f _) (push f l)) 'qss-ctest-o) l))))
    (qss-contract-remove 'qss-ctest-o)
    (should (equal (qss-ctest-o 5) 5))
    (should-not (qss-contract-of 'qss-ctest-o))))

(ert-deftest qss-contract-bad-specs ()
  (should-error (qss-contract-add 'car '(stringp)))
  (should-error (qss-contract-add 'car '(->)))
  (should-error (qss-contract-add 'car '(-> (opt stringp) integerp any)))
  (should-not (qss-contract-of 'car)))

(ert-deftest qss-contract-in-help ()
  (qss-ctest--with-fns (qss-ctest-help)
    (defun qss-ctest-help (x) "Return X." x)
    (qss-define-contract qss-ctest-help (-> integerp integerp))
    (save-window-excursion
      (describe-function 'qss-ctest-help)
      (with-current-buffer (help-buffer)
        (should (string-search "Contract: (-> integerp integerp)" (buffer-string)))))))

(ert-deftest qss-contract-keeps-docstring-of-generic ()
  "The docstring of cl-defgeneric survives the contract, later methods and removal."
  (qss-ctest--with-fns (qss-ctest-docgen)
    (cl-defgeneric qss-ctest-docgen (x) "Return X, documented.")
    (cl-defmethod qss-ctest-docgen ((x integer)) x)
    (qss-define-contract qss-ctest-docgen (-> any any))
    (should (string-prefix-p "Return X, documented." (documentation 'qss-ctest-docgen)))
    (cl-defmethod qss-ctest-docgen ((x string)) x)
    (should (string-prefix-p "Return X, documented." (documentation 'qss-ctest-docgen)))
    (qss-contract-remove 'qss-ctest-docgen)
    (should (string-prefix-p "Return X, documented." (documentation 'qss-ctest-docgen)))))

(ert-deftest qss-contract-keeps-docstring-of-function ()
  (qss-ctest--with-fns (qss-ctest-docfun)
    (defun qss-ctest-docfun (x) "Return X, also documented." x)
    (qss-define-contract qss-ctest-docfun (-> any any))
    (should (string-search "Return X, also documented." (documentation 'qss-ctest-docfun)))))

(provide 'qss-contract-test)
;;; qss-contract-test.el ends here
