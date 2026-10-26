;;; qss-elisp-xref-circular-test.el --- Lists that do not end  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The reader returns lists that contain themselves when a file says
;; so: '#1=(1 . #1#) in seq-tests.el made the walker loop for ever
;; (October 2026, while indexing GNU ELPA).  These tests read such
;; forms; a hang shows up as a run that does not end.
;;
;;   emacs --batch -L . -l qss-elisp-xref -l qss-elisp-xref-circular-test \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss-elisp-xref)

(ert-deftest qss-elisp-xref-elements-proper-dotted-circular ()
  (should (equal (qss-elisp-xref--elements '(a b c)) '(a b c)))
  (should (equal (qss-elisp-xref--elements '(a b . c)) '(a b c)))
  (should (equal (qss-elisp-xref--elements nil) nil))
  (let ((c (list 1 2 3)))
    (setcdr (last c) c)                 ; 1 2 3 1 2 3 ...
    (should (equal (qss-elisp-xref--elements c) '(1 2 3)))))

(defconst qss-elisp-xref-circular-test--source
  ";;; zirkel.el --- test  -*- lexical-binding: t -*-
(defun probe-cdr () (let ((x '#1=(1 . #1#))) x))
(defun probe-car () '#2=(a #2#))
(defun probe-vec () '#3=[a #3#])
(defun probe-dotted (a . rest) (list a rest))
(defun probe-normal (x) (seq-reduce #'+ x 0))
"
  "Forms like those in seq-tests.el, and two ordinary ones.")

(defun qss-elisp-xref-circular-test--index (source)
  "Index SOURCE as a file; return (FILE-ROW OCCURRENCES) from the database."
  (let* ((dir (make-temp-file "qss-xref-circ-" t))
         (file (expand-file-name "zirkel.el" dir))
         (db-file (expand-file-name "x.sqlite3" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert source))
          (let ((inhibit-message t))
            (qss-elisp-xref-run (list dir) db-file))
          (let ((db (sqlite-open db-file)))
            (unwind-protect
                (list (car (sqlite-select db "SELECT forms, error FROM files"))
                      (sqlite-select db "SELECT value, context, line FROM occurrences
                                         ORDER BY line, col"))
              (sqlite-close db))))
      (delete-directory dir t))))

(ert-deftest qss-elisp-xref-circular-forms-are-read ()
  "Every form is read, none fails, and the names are all there."
  (pcase-let ((`((,forms ,error) ,occ)
               (qss-elisp-xref-circular-test--index qss-elisp-xref-circular-test--source)))
    (should (= forms 5))
    (should (null error))
    ;; line 1 is the header, the definitions follow from line 2
    (cl-loop for name in '("probe-cdr" "probe-car" "probe-vec" "probe-dotted" "probe-normal")
             for line from 2
             do (should (member (list name "defining" line) occ)))))

(ert-deftest qss-elisp-xref-circular-elements-once ()
  "A circular list gives its elements once, as data."
  (pcase-let ((`(,_ ,occ) (qss-elisp-xref-circular-test--index qss-elisp-xref-circular-test--source)))
    ;; (a #2#) and [a #3#]: a once each, quoted
    (should (equal (cl-count-if (lambda (o) (equal o '("a" "quoted" 3))) occ) 1))
    (should (equal (cl-count-if (lambda (o) (equal o '("a" "quoted" 4))) occ) 1))))

(ert-deftest qss-elisp-xref-dotted-arglist ()
  "(a . rest): both are parameters, as before."
  (pcase-let ((`(,_ ,occ) (qss-elisp-xref-circular-test--index qss-elisp-xref-circular-test--source)))
    (should (member '("a" "parameter" 5) occ))
    (should (member '("rest" "parameter" 5) occ))))

(provide 'qss-elisp-xref-circular-test)
;;; qss-elisp-xref-circular-test.el ends here
