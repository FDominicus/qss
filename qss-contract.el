;;; qss-contract.el --- Contracts with blame, after Racket  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss, but depends on no other part of it.

;;; Commentary:

;; Contracts after the model of Racket (racket/contract): a contract
;; says what a function accepts and what it returns, and when it is
;; broken, the message says who broke it.
;;
;;   (qss-define-contract qss-sqlite
;;     (-> stringp (opt (or/c stringp symbolp)) qss-relation-p))
;;
;; The contract is an :around advice on the function.  It sees the
;; arguments before the function runs and the result before it
;; returns, so it knows whom to blame:
;;
;;   - an argument that breaks it: the caller;
;;   - a result that breaks it: the function itself (for a generic
;;     function: the method for the type of the first argument).
;;
;; The caller is named too: every contract binds the name of its
;; function while that function runs, so a broken contract further in
;; names the checked function it was called from.  Checked functions
;; are the boundaries, as modules are in Racket.
;;
;; The contract stands beside the code, not in it, as with
;; provide/contract: a file can hold the whole checked interface of a
;; package.  It works for generic functions (and every method added
;; later) and survives redefinition.  C-h f shows it.
;;
;; The language of contracts (data, read by `qss-define-contract'):
;;
;;   PRED              a predicate: stringp, qss-table-p, natnump ...
;;   any               anything
;;   NAME              a contract named with `qss-define-contract-type'
;;   (or/c C ...)      one of them        (and/c C ...)  all of them
;;   (not/c C)         not C
;;   (one-of V ...)    one of the values V (compared with `equal')
;;   (listof C)        a list whose elements satisfy C
;;   (list/c C ...)    a list of exactly these elements
;;   (cons/c CAR CDR)  a cons
;;   (lambda (x) ...)  a predicate written in place
;;   (-> ARG ... RESULT)   a function; in ARG, (opt C) is an optional
;;                     argument (nil is always allowed for it, as nil
;;                     means "not given" in Lisp) and (rest C) the
;;                     remaining arguments, each satisfying C
;;
;; A function contract as an argument (a function passed in) checks
;; only that it is a function; the function itself is not wrapped.
;;
;; `qss-contract-enabled' turns the checks off and on; the advice stays
;; and then only passes the call on.  `qss-contract-remove' removes one.

;;; Code:

(require 'cl-lib)
(require 'nadvice)
(require 'subr-x)                       ; hash-table-keys

(defgroup qss-contract nil
  "Contracts with blame, after Racket."
  :group 'lisp
  :prefix "qss-contract-")

(defcustom qss-contract-enabled t
  "Non-nil means contracts are checked.
When nil, a checked function costs one extra call and nothing else."
  :type 'boolean
  :group 'qss-contract)

(define-error 'qss-contract-violation "Contract violation")

(defvar qss-contract--table (make-hash-table :test #'eq)
  "Function -> (SPEC ARG-CHECKS RESULT-CHECK ADVICE).")

(defvar qss-contract--current nil
  "The checked function running now, the caller of the next checked one.")

;;; ------------------------------------------------------------------
;;; Compiling contracts to checks
;;
;; A check is a function of a value that returns nil if the value
;; satisfies the contract, otherwise a string with details ("" if
;; there is nothing to add).

(defun qss-contract--show (value)
  "Return VALUE printed short, for a message."
  (let ((print-length 6) (print-level 3) (print-escape-newlines t))
    (truncate-string-to-width (prin1-to-string value) 120 nil nil "...")))

(defun qss-define-contract-type (name spec &optional doc)
  "Name the contract SPEC as NAME, for use in other contracts.
NAME is looked up when a value is checked, so it can be redefined.
DOC describes it."
  (put name 'qss-contract-type (cons spec (qss-contract--compile spec)))
  (when doc (put name 'qss-contract-type-doc doc))
  name)

(defun qss-contract--compile (spec)
  "Return the check for the contract SPEC; signal an error if SPEC is unknown."
  (pcase spec
    ('any #'ignore)
    ((and (pred symbolp) name (guard (get name 'qss-contract-type)))
     (lambda (v) (funcall (cdr (get name 'qss-contract-type)) v)))
    ((pred symbolp)
     (unless (fboundp spec) (error "Unknown contract: %S (neither a predicate nor a named contract)" spec))
     (lambda (v) (and (not (funcall spec v)) "")))
    (`(lambda . ,_)
     (let ((pred (eval spec t)))
       (lambda (v) (and (not (funcall pred v)) ""))))
    (`(or/c . ,specs)
     (let ((checks (mapcar #'qss-contract--compile specs)))
       (lambda (v) (and (cl-every (lambda (c) (funcall c v)) checks) ""))))
    (`(and/c . ,specs)
     (let ((checks (cl-mapcar #'cons specs (mapcar #'qss-contract--compile specs))))
       (lambda (v)
         (cl-some (lambda (c)
                    (let ((d (funcall (cdr c) v)))
                      (and d (format "not %S%s" (car c) (if (string-empty-p d) "" (concat ": " d))))))
                  checks))))
    (`(not/c ,inner)
     (let ((check (qss-contract--compile inner)))
       (lambda (v) (and (not (funcall check v)) ""))))
    (`(one-of . ,values)
     (lambda (v) (and (not (member v values)) "")))
    (`(listof ,inner)
     (let ((check (qss-contract--compile inner)))
       (lambda (v)
         (if (not (proper-list-p v))
             "not a list"
           (let ((i 0) (detail nil))
             (while (and v (not detail))
               (let ((d (funcall check (car v))))
                 (when d
                   (setq detail (format "element %d is %s, not %S%s" i (qss-contract--show (car v))
                                        inner (if (string-empty-p d) "" (concat ": " d))))))
               (setq v (cdr v) i (1+ i)))
             detail)))))
    (`(list/c . ,specs)
     (let ((checks (mapcar #'qss-contract--compile specs)))
       (lambda (v)
         (cond ((not (proper-list-p v)) "not a list")
               ((/= (length v) (length checks))
                (format "%d elements, not %d" (length v) (length checks)))
               (t (cl-loop for x in v for c in checks for s in specs for i from 0
                           for d = (funcall c x)
                           when d return (format "element %d is %s, not %S" i (qss-contract--show x) s)))))))
    (`(cons/c ,car-spec ,cdr-spec)
     (let ((car-check (qss-contract--compile car-spec))
           (cdr-check (qss-contract--compile cdr-spec)))
       (lambda (v)
         (cond ((not (consp v)) "not a cons")
               ((funcall car-check (car v)) (format "its car is not %S" car-spec))
               ((funcall cdr-check (cdr v)) (format "its cdr is not %S" cdr-spec))))))
    (`(-> . ,_)
     (lambda (v) (and (not (functionp v)) "not a function")))
    (_ (error "Unknown contract: %S" spec))))

(defun qss-contract--parse-function (spec)
  "Return (ARGS REST RESULT) for the function contract SPEC.
ARGS is a list of (SPEC CHECK OPTIONAL), REST a (SPEC . CHECK) or nil,
RESULT a (SPEC . CHECK)."
  (unless (eq (car-safe spec) '->)
    (error "A function contract starts with ->: %S" spec))
  (let* ((parts (cdr spec))
         (result (car (last parts)))
         (args nil) (rest nil) (seen-opt nil))
    (unless parts (error "A function contract needs a result: %S" spec))
    (dolist (a (butlast parts))
      (pcase a
        (`(opt ,c) (setq seen-opt t)
         (let ((check (qss-contract--compile c)))
           (push (list a (lambda (v) (and v (funcall check v))) t) args)))
        (`(rest ,c) (when rest (error "Two rest arguments in %S" spec))
         (setq rest (cons c (qss-contract--compile c))))
        (_ (when (or seen-opt rest)
             (error "A required argument after optional ones in %S" spec))
           (push (list a (qss-contract--compile a) nil) args))))
    (list (nreverse args) rest (cons result (qss-contract--compile result)))))

;;; ------------------------------------------------------------------
;;; Checking

(defun qss-contract--generic-p (fn)
  "Return non-nil if FN names a generic function."
  (and (fboundp 'cl--generic) (cl--generic fn)))

(defun qss-contract--arg-name (orig index)
  "Return the name of argument INDEX of the function ORIG, or nil."
  (let ((arglist (ignore-errors (help-function-arglist orig t))))
    (when (listp arglist)
      (nth index (cl-remove-if (lambda (a) (memq a '(&optional &rest))) arglist)))))

(defun qss-contract--violation (fn spec what value detail blame)
  "Signal that VALUE, WHAT of FN, breaks SPEC; DETAIL says more; BLAME is to blame."
  (signal 'qss-contract-violation
          (list (format "%s: %s must satisfy %S, got %s%s\n  blame: %s\n  contract: %S"
                        fn what (car spec) (qss-contract--show value)
                        (if (or (null detail) (string-empty-p detail)) "" (concat " (" detail ")"))
                        blame (qss-contract-of fn)))))

(defun qss-contract--check-args (fn orig args arg-checks rest)
  "Check ARGS of FN (whose definition is ORIG) against ARG-CHECKS and REST."
  (let ((i 0) (checks arg-checks))
    (while args
      (let* ((arg (car args))
             (entry (car checks))
             (spec (if entry (car entry) (car rest)))
             (check (if entry (nth 1 entry) (cdr rest)))
             (d (and check (funcall check arg))))
        (when d
          (qss-contract--violation
           fn (list (if (and entry (nth 2 entry)) (cadr spec) spec))
           (let ((name (and (not (qss-contract--generic-p fn)) (qss-contract--arg-name orig i))))
             (if name (format "argument %d (%s)" (1+ i) name) (format "argument %d" (1+ i))))
           arg d
           (if qss-contract--current
               (format "the caller, %s" qss-contract--current)
             "the caller (outside the checked functions)")))
        (setq args (cdr args) checks (cdr checks) i (1+ i))))))

(defun qss-contract--make-advice (fn arg-checks rest result)
  "Return the :around advice that checks the contract of FN."
  (lambda (orig &rest args)
    (if (not qss-contract-enabled)
        (apply orig args)
      (qss-contract--check-args fn orig args arg-checks rest)
      (let ((value (let ((qss-contract--current fn)) (apply orig args))))
        (let ((d (funcall (cdr result) value)))
          (when d
            (qss-contract--violation
             fn (list (car result)) "the result" value d
             (if (and (qss-contract--generic-p fn) args)
                 (format "%s itself, the method for %s" fn (type-of (car args)))
               (format "%s itself" fn)))))
        value))))

;;; ------------------------------------------------------------------
;;; Defining

(defun qss-contract-add (fn spec)
  "Put the function contract SPEC on the function FN (a symbol).
A contract FN had before is replaced.  See `qss-define-contract'."
  (qss-contract-remove fn)
  (pcase-let* ((`(,args ,rest ,result) (qss-contract--parse-function spec))
               (advice (qss-contract--make-advice fn args rest result)))
    (advice-add fn :around advice '((name . qss-contract) (depth . -90)))
    (puthash fn (list spec args rest result advice) qss-contract--table)
    fn))

(defmacro qss-define-contract (fn spec)
  "Put the contract SPEC on the function FN, an unquoted symbol.
SPEC is a function contract (-> ARG ... RESULT); see qss-contract.el
for the language.  FN need not be defined yet; it is checked once it
is."
  `(qss-contract-add ',fn ',spec))

(defun qss-contract-remove (fn)
  "Remove the contract of FN, if it has one."
  (let ((entry (gethash fn qss-contract--table)))
    (when entry
      (advice-remove fn (nth 4 entry))
      (remhash fn qss-contract--table))))

(defun qss-contract-remove-all ()
  "Remove all contracts."
  (interactive)
  (dolist (fn (hash-table-keys qss-contract--table))
    (qss-contract-remove fn)))

(defun qss-contract-of (fn)
  "Return the contract of FN, or nil."
  (car (gethash fn qss-contract--table)))

(defun qss-contract-functions ()
  "Return the functions with a contract, sorted by name."
  (sort (hash-table-keys qss-contract--table) #'string<))

(defun qss-contract-toggle ()
  "Turn the checking of contracts off or on."
  (interactive)
  (setq qss-contract-enabled (not qss-contract-enabled))
  (message "Contracts are %s" (if qss-contract-enabled "checked" "not checked")))

;;; C-h f shows the contract

(defun qss-contract--describe (function)
  "Insert the contract of FUNCTION into its help, if it has one."
  (let ((spec (qss-contract-of function)))
    (when spec
      (insert "\n  Contract: " (prin1-to-string spec)
              (if qss-contract-enabled "" "  (not checked now)")
              "\n"))))

(add-hook 'help-fns-describe-function-functions #'qss-contract--describe)

(provide 'qss-contract)
;;; qss-contract.el ends here
