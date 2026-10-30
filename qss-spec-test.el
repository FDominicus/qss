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

(declare-function qss-odbc-execute "qss-odbc")
(declare-function qss-odbc-available-p "qss-odbc")

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
                     ('sqlite `(qss-sqlite ,file ,(substring (symbol-name name) 1)))
                     ;; FILE is the DSN; the tables are named qss_spec_NAME
                     ('odbc `(qss-odbc-source ,file ,(qss-spec-test--odbc-name name))))))
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

(defun qss-spec-test--odbc-name (key)
  "The name of the table KEY (:people) in an ODBC test database."
  (concat "qss_spec_" (substring (symbol-name key) 1)))

(defun qss-spec-test-odbc-failures (dsn &optional only)
  "Run the shared cases against the ODBC data source DSN.
The tables are written as qss_spec_people ... and dropped afterwards.
Return a list of (NAME odbc MESSAGE)."
  (pcase-let* ((`(,tables . ,cases) (qss-spec-test--cases))
               (db (qss-sql-odbc-database dsn))
               (con (qss-sql-odbc-connection db))
               (failures nil))
    (unwind-protect
        (progn
          (dolist (tab tables)
            (qss-sql-odbc-write (qss-as-table (cadr tab)) db
                                (qss-spec-test--odbc-name (car tab)) 'replace))
          (dolist (c cases)
            (when (or (null only) (string-search only (cadr c)))
              (let ((msg (qss-spec-test--run c 'odbc tables dsn)))
                (when msg (push (list (cadr c) 'odbc msg) failures))))))
      (dolist (tab tables)
        (qss-odbc-execute con (format "DROP TABLE IF EXISTS %s"
                                      (qss-sql-quote (qss-spec-test--odbc-name (car tab))))))
      (qss-odbc-execute con (format "DELETE FROM %s WHERE \"table\" LIKE 'qss_spec_%%'"
                                    (qss-sql-quote qss-sql--types-table)))
      (qss-odbc-refresh dsn))
    (nreverse failures)))

(ert-deftest qss-spec-cases-odbc ()
  "The shared cases against each DSN of QSS_ODBC_DSNS (default \"test\").
Needs the package qss-odbc with its module built."
  (skip-unless (and (require 'qss-odbc nil t) (qss-odbc-available-p)))
  (let ((ran nil))
    (dolist (dsn (split-string (or (getenv "QSS_ODBC_DSNS") "test")))
      (when (condition-case err (qss-sql-odbc-database dsn)
              (error (message "qss-spec-cases-odbc: %s unreachable: %s"
                              dsn (error-message-string err))
                     nil))
        (setq ran t)
        (ert-info ((format "DSN %s" dsn))
          (let ((failures (qss-spec-test-odbc-failures dsn)))
            (should (equal "" (mapconcat (lambda (f) (format "%s: %s" (nth 0 f) (nth 2 f)))
                                         failures "\n")))))))
    (unless ran (message "qss-spec-cases-odbc: skipped, no DSN reachable"))
    (skip-unless ran)))

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

;;; ------------------------------------------------------------------
;;; Nushell: the model itself
;;
;; Each case becomes a Nushell pipeline (`qss-spec-test--nu-script') and
;; runs in nu.  A case without a :nushell note must give the expected
;; result there too; a case noted (:differs ...) must give something
;; else (otherwise the note is out of date); (:skip ...) is not run.
;; Cases Nushell cannot write (like, head, group-by ...) are noted.

(define-error 'qss-spec-test-no-nu "No Nushell form for this")

(defun qss-spec-test--nu-name (key)
  "Return the column KEY (a keyword) as a Nushell name."
  (substring (symbol-name key) 1))

(defun qss-spec-test--nu-cell (key)
  "Return the column KEY of the row $r as a Nushell expression."
  (format "$r.%s" (qss-nuon--string (qss-spec-test--nu-name key))))

(defun qss-spec-test--nu-value (v)
  "Return the value V as a Nushell literal."
  (cond ((null v) "null")
        ((eq v t) "true")
        ((stringp v) (qss-nuon--string v))
        ((integerp v) (number-to-string v))
        ((floatp v) (qss-nuon--float v))
        ((and (consp v) (eq (car v) 'quote))
         (format "[%s]" (mapconcat #'qss-spec-test--nu-value (cadr v) ", ")))
        (t (signal 'qss-spec-test-no-nu (list v)))))

(defun qss-spec-test--nu-condition (form)
  "Return the condition FORM as a Nushell expression over $r."
  (cond ((keywordp form) (qss-spec-test--nu-cell form))
        ((atom form) (qss-spec-test--nu-value form))
        ((eq (car form) 'quote) (qss-spec-test--nu-value form))
        (t
         (let ((args (mapcar #'qss-spec-test--nu-condition (cdr form))))
           (pcase (car form)
             ('= (format "(%s == %s)" (nth 0 args) (nth 1 args)))
             ('/= (format "(%s != %s)" (nth 0 args) (nth 1 args)))
             ((and op (or '< '> '<= '>=)) (format "(%s %s %s)" (nth 0 args) op (nth 1 args)))
             ('and (format "(%s)" (mapconcat #'identity args " and ")))
             ('or (format "(%s)" (mapconcat #'identity args " or ")))
             ('not (format "(not %s)" (nth 0 args)))
             ('is-null (format "(%s == null)" (nth 0 args)))
             ('not-null (format "(%s != null)" (nth 0 args)))
             ('in (format "(%s in %s)" (nth 0 args) (nth 1 args)))
             (_ (signal 'qss-spec-test-no-nu (list form))))))))

(defun qss-spec-test--nu-join (args)
  "Return the Nushell join for the stage arguments ARGS of `join'."
  (let* ((other (car args))
         (opts (cdr args))
         (on (plist-get opts :on))
         (kind (plist-get opts :kind))
         (suffix (plist-get opts :suffix))
         (prefix (plist-get opts :prefix)))
    (unless (and (eq (car-safe other) 'table) on
                 (or (keywordp on) (and (consp on) (= (length on) 2) (cl-every #'keywordp on))))
      (signal 'qss-spec-test-no-nu (list args)))
    (concat "join"
            (if (memq kind '(:left left)) " --left" "")
            (if suffix (format " --suffix %s" (qss-nuon--string suffix)) "")
            (if prefix (format " --prefix %s" (qss-nuon--string prefix)) "")
            (format " $%s " (qss-spec-test--nu-name (cadr other)))
            (if (keywordp on)
                (qss-spec-test--nu-name on)
              (format "%s %s" (qss-spec-test--nu-name (car on)) (qss-spec-test--nu-name (cadr on)))))))

(defun qss-spec-test--nu-stage (stage)
  "Return STAGE as a Nushell command."
  (let* ((stage (if (consp stage) stage (list stage)))
         (args (cdr stage))
         (names (lambda () (mapconcat #'qss-spec-test--nu-name args " "))))
    (pcase (car stage)
      ((or 'where 'filter) (format "where {|r| %s}" (qss-spec-test--nu-condition (car args))))
      ('select (concat "select " (funcall names)))
      ('reject (concat "reject " (funcall names)))
      ('get (concat "get " (funcall names)))
      ('sort-by
       ;; one sort per column, the last first: Nushell sorts stably
       (mapconcat (lambda (k) (format "sort-by %s%s" (qss-spec-test--nu-name (car k)) (if (cdr k) " -r" "")))
                  (reverse (mapcar (lambda (k) (cons (intern (format ":%s" (car k))) (cdr k)))
                                   (qss--sort-keys args)))
                  " | "))
      ((and w (or 'first 'last)) (if args (format "%s %d" w (car args)) (symbol-name w)))
      ((or 'count 'length) "length")
      ('columns "columns")
      ('join (qss-spec-test--nu-join args))
      ('group-by (concat "group-by " (funcall names) " --to-table"))
      (_ (signal 'qss-spec-test-no-nu (list stage))))))

(defun qss-spec-test--nu-script (case tables)
  "Return the Nushell script of CASE over TABLES."
  (let ((p (cddr case)))
    (concat
     (mapconcat (lambda (tab)
                  (format "let %s = %s\n" (qss-spec-test--nu-name (car tab))
                          (qss-nuon-encode (cadr tab))))
                tables "")
     (mapconcat #'identity
                (append (list (concat "$" (qss-spec-test--nu-name (plist-get p :from))))
                        (mapcar #'qss-spec-test--nu-stage (plist-get p :stages))
                        (list "to nuon"))
                " | "))))

(defun qss-spec-test--from-nu (v)
  "Return the NUON value V as the cases write results: records as plists."
  (cond ((eq v :null) nil)
        ((and (consp v) (consp (car v)) (symbolp (caar v)) (not (keywordp (caar v))))
         ;; a record: an alist
         (cl-mapcan (lambda (cell) (list (intern (format ":%s" (car cell)))
                                         (qss-spec-test--from-nu (cdr cell))))
                    v))
        ((listp v) (mapcar #'qss-spec-test--from-nu v))
        (t v)))

(defun qss-spec-test--run-nu (case tables)
  "Run CASE in Nushell.  Return (STATUS . MESSAGE): STATUS is ok, fail or no-nu."
  (let* ((p (cddr case))
         (script (condition-case nil (qss-spec-test--nu-script case tables)
                   ((qss-spec-test-no-nu user-error) nil))))
    (if (not script)
        (cons 'no-nu "no Nushell form")
      (with-temp-buffer
        (let* ((code (call-process "nu" nil t nil "-c" script))
               (out (string-trim (buffer-string)))
               (got (and (zerop code) (qss-spec-test--from-nu (qss-nuon-read out))))
               (columns (eq (qss-spec-test--word (car (last (plist-get p :stages)))) 'columns))
               (got (if columns (mapcar (lambda (c) (intern (format ":%s" c))) got) got))
               (expected (cond ((plist-member p :rows) (plist-get p :rows))
                               ((plist-member p :values) (plist-get p :values))
                               (t (plist-get p :value)))))
          (cond ((plist-get p :error)
                 (if (zerop code) (cons 'fail (format "expected an error, got %S" got)) (cons 'ok nil)))
                ((not (zerop code)) (cons 'fail (format "nu failed: %s" out)))
                ((qss-spec-test--same-p expected got) (cons 'ok nil))
                (t (cons 'fail (format "expected %S\n      got %S" expected got)))))))))

(defun qss-spec-test-nushell-report (&optional only)
  "Run the shared cases in Nushell (with ONLY: names containing it).
Return a list of (NAME VERDICT MESSAGE); VERDICT is one of
  agrees        no note, same result
  disagrees     no note, other result (a finding)
  differs       noted :differs, other result (as noted)
  note-stale    noted :differs, but the same result now
  skipped       noted :skip
  cannot        no Nushell form for it (and no note)"
  (pcase-let ((`(,tables . ,cases) (qss-spec-test--cases))
              (report nil))
    (dolist (c cases)
      (when (or (null only) (string-search only (cadr c)))
        (let* ((note (plist-get (cddr c) :nushell))
               (r (and (not (eq (car note) :skip)) (qss-spec-test--run-nu c tables)))
               (verdict (cond ((eq (car note) :skip) 'skipped)
                              ((eq (car r) 'no-nu) (if note 'differs 'cannot))
                              ((eq (car note) :differs) (if (eq (car r) 'ok) 'note-stale 'differs))
                              ((eq (car r) 'ok) 'agrees)
                              (t 'disagrees))))
          (push (list (cadr c) verdict (cdr r)) report))))
    (nreverse report)))

(ert-deftest qss-spec-nushell ()
  "The shared cases in Nushell: the same results, or a note says why not."
  (skip-unless (executable-find "nu"))
  (let ((bad (seq-filter (lambda (r) (memq (nth 1 r) '(disagrees note-stale cannot)))
                         (qss-spec-test-nushell-report))))
    (should (equal "" (mapconcat (lambda (r) (format "%s: %s %s" (nth 0 r) (nth 1 r) (or (nth 2 r) "")))
                                 bad "\n")))))

(provide 'qss-spec-test)
;;; qss-spec-test.el ends here
