;;; qss-spec-test.el --- The shared cases of spec/cases.sexp  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; spec/vocabulary.sexp says what the words of qss mean, spec/cases.sexp
;; shows it with data.  The same files are run by qss-query in Common
;; Lisp (qss-query/spec/).  Each case is a pipe of words; here it becomes
;; (qss-> ROWS STAGE ...), once on the rows in memory and once on the
;; same tables written to a fresh SQLite file.
;;
;;   (qss-spec-test-failures)          ; => list of (NAME MODE MESSAGE)
;;   (qss-spec-test-failures "join")

;;; Code:

(require 'ert)
(require 'qss)

(defconst qss-spec-test--dir
  (expand-file-name "spec/" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the shared files.")

(defun qss-spec-test--read (name)
  "Return the form in the shared file NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name qss-spec-test--dir))
    (goto-char (point-min))
    (read (current-buffer))))

(defun qss-spec-test--cases ()
  "Return (TABLES . CASES) from spec/cases.sexp."
  (let ((form (qss-spec-test--read "cases.sexp")))
    (cl-assert (and (eq (car form) :cases) (eql (cadr form) 1)))
    (let ((rest (cddr form)))
      (cons (plist-get rest :tables)
            (seq-filter (lambda (x) (and (consp x) (eq (car x) :case))) rest)))))

(defun qss-spec-test--write-tables (tables file)
  "Write TABLES (a list of (NAME ROWS)) into the SQLite FILE."
  (dolist (tab tables)
    (qss-sql-write (qss-as-table (cadr tab)) file
                   (substring (symbol-name (car tab)) 1) 'replace)))

(defun qss-spec-test--word (stage)
  "Return the word of STAGE as a symbol."
  (if (consp stage) (car stage) stage))

(defun qss-spec-test--replace-tables (form fn)
  "Return FORM with every (table NAME) replaced by (FN NAME)."
  (cond ((and (consp form) (eq (car form) 'table)) (funcall fn (cadr form)))
        ((consp form) (cons (qss-spec-test--replace-tables (car form) fn)
                            (qss-spec-test--replace-tables (cdr form) fn)))
        (t form)))

(defun qss-spec-test--form (from stages mode tables file)
  "Return the code of the pipe FROM STAGES in MODE (memory or sqlite)."
  (let* ((source (lambda (name)
                   (pcase mode
                     ('memory `',(cadr (assq name tables)))
                     ('sqlite `(qss-sqlite ,file ,(substring (symbol-name name) 1))))))
         (stages (qss-spec-test--replace-tables stages source))
         (word (and stages (qss-spec-test--word (car (last stages)))))
         (pipe `(qss-> ,(funcall source from) ,@stages)))
    (pcase word
      ((or 'count 'length) pipe)
      ('columns `(mapcar (lambda (c) (intern (format ":%s" c))) (qss-collect ,pipe)))
      ('collect pipe)
      (_ `(qss-collect ,pipe)))))

(defun qss-spec-test--same-p (a b)
  "Return non-nil if A and B are the same: numbers by value, else `equal'."
  (cond ((and (numberp a) (numberp b)) (= a b))
        ((and (consp a) (consp b))
         (and (qss-spec-test--same-p (car a) (car b))
              (qss-spec-test--same-p (cdr a) (cdr b))))
        (t (equal a b))))

(defun qss-spec-test--run (case mode tables file)
  "Return nil if CASE passes in MODE, else a message."
  (let* ((p (cddr case))
         (form (qss-spec-test--form (plist-get p :from) (plist-get p :stages) mode tables file))
         (expected (cond ((plist-member p :rows) (plist-get p :rows))
                         ((plist-member p :values) (plist-get p :values))
                         (t (plist-get p :value))))
         got err)
    (condition-case e
        (setq got (eval form t))
      (error (setq err e)))
    (cond ((plist-get p :error)
           (unless err (format "expected an error, got %S" got)))
          (err (format "error: %s" (error-message-string err)))
          ((not (qss-spec-test--same-p expected got))
           (format "expected %S\n      got %S" expected got)))))

(defun qss-spec-test-failures (&optional only)
  "Run the shared cases (with ONLY: those whose name contains it).
In memory and in SQLite.  Return a list of (NAME MODE MESSAGE)."
  (pcase-let* ((`(,tables . ,cases) (qss-spec-test--cases))
               (file (make-temp-file "qss-spec-" nil ".sqlite3"))
               (failures nil))
    (unwind-protect
        (progn
          (delete-file file)
          (qss-spec-test--write-tables tables file)
          (dolist (c cases)
            (when (or (null only) (string-search only (cadr c)))
              (dolist (mode '(memory sqlite))
                (let ((msg (qss-spec-test--run c mode tables file)))
                  (when msg (push (list (cadr c) mode msg) failures)))))))
      (when (file-exists-p file) (delete-file file)))
    (nreverse failures)))

(ert-deftest qss-spec-cases ()
  "The shared cases of spec/cases.sexp, in memory and in SQLite."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((failures (qss-spec-test-failures)))
    (should (equal "" (mapconcat (lambda (f) (format "%s (%s): %s" (nth 0 f) (nth 1 f) (nth 2 f)))
                                 failures "\n")))))

(ert-deftest qss-spec-vocabulary ()
  "Every word of spec/vocabulary.sexp that Emacs Lisp has is a function and a word of `qss->'."
  (let ((form (qss-spec-test--read "vocabulary.sexp")))
    (should (eq :vocabulary (car form)))
    (dolist (w (cddr form))
      (let ((p (cdr w)))
        (when (memq (plist-get p :status) '(:both :elisp))
          (should (fboundp (plist-get p :elisp)))
          (should (assq (plist-get p :word) qss--words)))))))

(provide 'qss-spec-test)
;;; qss-spec-test.el ends here
