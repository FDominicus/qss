;;; qss-elisp-xref.el --- Cross references of Emacs Lisp, in SQLite  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  It needs only Emacs: the qss words are
;; for reading the result, not for making it.

;;; Commentary:

;; Reads the Emacs Lisp files installed (Emacs itself, ELPA/MELPA
;; packages, your own) and writes into an SQLite file what they
;; define and where every symbol and every string appears:
;;
;;   M-x qss-elisp-xref-index      runs in an Emacs of its own (batch)
;;
;; Nothing is loaded or run to do that: the Lisp reader reads the
;; forms, with the position of every symbol (`read-positioning-symbols',
;; Emacs 29).  The calls that the byte compiler sees come in a second
;; step (`qss-elisp-xref-calls-run'), in an Emacs of its own: that one
;; compiles the files, into a directory thrown away afterwards.
;;
;; The tables (see `qss-elisp-xref--schema'):
;;
;;   meta         the Emacs version and when the index was made
;;   files        one row per file: path, library, origin (emacs, elpa,
;;                user), package and version, the Commentary, sha1
;;   forms        every top-level form with its source text
;;   definitions  defun, defmacro, defvar, defcustom, cl-defgeneric,
;;                cl-defmethod, cl-defstruct, local cl-flet/cl-labels
;;                ... with arglist and the whole docstring; also the
;;                primitives and special forms of C (without a file)
;;   occurrences  every symbol and every string (not docstrings, not
;;                nil and t) with line and column, the definition it is
;;                in, the function of the list around it (parent_head)
;;                and its place there (arg_index), and a context:
;;                  head        first in a list (a call, a macro, a binding ...)
;;                  macro-use   head, and a macro by the definitions
;;                  special-form head, and a special form
;;                  function    #'name
;;                  quoted      inside ' or ` (data)
;;                  defining    the name a definition defines
;;                  parameter   in an argument list
;;                  binding     a variable bound by let and the like
;;                  other       any other symbol (mostly variables)
;;                  string      a string
;;   calls        caller, callee: what the byte compiler saw (exact,
;;                but without macros and without the primitives the
;;                byte code knows, such as car and eq; caller NULL is
;;                the top level, which includes cl-defmethod bodies;
;;                make-closure is the compiler's own)
;;   compiled     the files compiled for calls, and why not
;;                (no-byte-compile, an error)
;;
;; "head" is what the reader can see: the first symbol of a list.  It
;; is a call most of the time, but not always (pcase patterns, cond
;; clauses with a symbol ...); "calls" is the exact version.
;;
;; A second run reads only the files that changed (by sha1) and drops
;; those that are gone.  Read the result with qss:
;;
;;   (sqlite "~/.emacs.d/elisp-xref.sqlite3" "occurrences")
;;     (where (= :value "seq-reduce")) ...

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'lisp-mnt)
(require 'bytecomp)
(require 'subr-x)
(require 'help)                         ; help-function-arglist

(defgroup qss-elisp-xref nil
  "Cross references of Emacs Lisp, in SQLite."
  :group 'qss
  :prefix "qss-elisp-xref-")

(defcustom qss-elisp-xref-database nil
  "The SQLite file of the cross references.
nil means elisp-xref.sqlite3 in `qss-data-directory' (when qss is
loaded) or else in `user-emacs-directory'.  Keep it on a local disk:
SQLite over a network drive is not safe when two Emacs use it."
  :type '(choice (const :tag "elisp-xref.sqlite3 in qss-data-directory" nil) file))

(defvar qss-data-directory)             ; qss-sql.el; this file stands alone

(defun qss-elisp-xref-database-file ()
  "The cross reference database, as an absolute file name.
See `qss-elisp-xref-database'."
  (expand-file-name
   (or qss-elisp-xref-database
       (expand-file-name "elisp-xref.sqlite3"
                         (or (bound-and-true-p qss-data-directory) user-emacs-directory)))))

(defcustom qss-elisp-xref-directories nil
  "Directories to index, searched recursively.
nil means: the Lisp directory of this Emacs and `package-user-dir'."
  :type '(repeat directory))

(defcustom qss-elisp-xref-skip "\\(?:-autoloads\\|-pkg\\|loaddefs\\|ldefs-boot\\|\\.dir-locals\\)\\.el"
  "Regexp of file names not to index (generated files)."
  :type 'regexp)

(defconst qss-elisp-xref-format 3
  "Version of the tables; a file of another version is made anew.")

;;; ------------------------------------------------------------------
;;; The tables

(defconst qss-elisp-xref--schema
  '("CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)"
    "CREATE TABLE IF NOT EXISTS files (
       id INTEGER PRIMARY KEY, path TEXT UNIQUE, library TEXT, origin TEXT,
       package TEXT, package_version TEXT, lexical INTEGER, commentary TEXT,
       mtime TEXT, sha1 TEXT, forms INTEGER, error TEXT)"
    "CREATE TABLE IF NOT EXISTS forms (
       id INTEGER PRIMARY KEY, file_id INTEGER, head TEXT,
       start_line INTEGER, end_line INTEGER, text TEXT)"
    "CREATE TABLE IF NOT EXISTS definitions (
       id INTEGER PRIMARY KEY, form_id INTEGER, file_id INTEGER, parent_id INTEGER,
       name TEXT, kind TEXT, specializers TEXT, arglist TEXT, interactive INTEGER,
       doc TEXT, line INTEGER)"
    "CREATE TABLE IF NOT EXISTS occurrences (
       value TEXT, context TEXT, file_id INTEGER, form_id INTEGER, definition_id INTEGER,
       line INTEGER, col INTEGER, parent_head TEXT, arg_index INTEGER)"
    "CREATE TABLE IF NOT EXISTS calls (caller TEXT, callee TEXT, file_id INTEGER)"
    "CREATE TABLE IF NOT EXISTS compiled (file_id INTEGER PRIMARY KEY, error TEXT)"
    "CREATE TABLE IF NOT EXISTS summaries (
       name TEXT PRIMARY KEY, kind TEXT, arglist TEXT, summary TEXT, uses INTEGER, files INTEGER)"
    "CREATE TABLE IF NOT EXISTS categories (name TEXT, category TEXT, sources TEXT)"
    "CREATE INDEX IF NOT EXISTS cat_category ON categories(category)"
    "CREATE INDEX IF NOT EXISTS cat_name ON categories(name)"
    "CREATE INDEX IF NOT EXISTS occ_value ON occurrences(value)"
    "CREATE INDEX IF NOT EXISTS occ_file ON occurrences(file_id)"
    "CREATE INDEX IF NOT EXISTS occ_def ON occurrences(definition_id)"
    "CREATE INDEX IF NOT EXISTS def_name ON definitions(name)"
    "CREATE INDEX IF NOT EXISTS def_file ON definitions(file_id)"
    "CREATE INDEX IF NOT EXISTS form_file ON forms(file_id)"
    "CREATE INDEX IF NOT EXISTS calls_callee ON calls(callee)"
    "CREATE INDEX IF NOT EXISTS calls_caller ON calls(caller)")
  "The SQL that makes the tables.")

(defun qss-elisp-xref--open (file)
  "Open the database FILE, creating the tables; return the connection."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (let ((db (sqlite-open (expand-file-name file))))
    (let ((format (ignore-errors
                    (caar (sqlite-select db "SELECT value FROM meta WHERE key = 'format'")))))
      (when (and format (not (equal format (number-to-string qss-elisp-xref-format))))
        ;; another layout of the tables: start anew
        (dolist (table '("meta" "files" "forms" "definitions" "occurrences" "calls" "compiled"))
          (sqlite-execute db (format "DROP TABLE IF EXISTS %s" table)))))
    (dolist (sql qss-elisp-xref--schema) (sqlite-execute db sql))
    (sqlite-pragma db "journal_mode = WAL")
    (sqlite-pragma db "synchronous = NORMAL")
    db))

(defun qss-elisp-xref--next-id (db table)
  "Return the next free id of TABLE in DB."
  (1+ (or (caar (sqlite-select db (format "SELECT max(id) FROM %s" table))) 0)))

;;; ------------------------------------------------------------------
;;; Which files

(defun qss-elisp-xref--lisp-directory ()
  "Return the directory of the Lisp files of this Emacs."
  (let ((subr (locate-library "subr")))
    (file-name-directory (directory-file-name (file-name-directory subr)))))

(defun qss-elisp-xref--directories ()
  "Return the directories to index."
  (or qss-elisp-xref-directories
      (delq nil (list (qss-elisp-xref--lisp-directory)
                      (and (boundp 'package-user-dir) (file-directory-p package-user-dir)
                           package-user-dir)))))

(defun qss-elisp-xref--files (dirs)
  "Return the Emacs Lisp files under DIRS, sorted, without generated ones."
  (let (files)
    (dolist (dir dirs)
      (dolist (f (directory-files-recursively dir "\\.el\\(?:\\.gz\\)?\\'"))
        (unless (string-match-p qss-elisp-xref-skip (file-name-nondirectory f))
          (push (expand-file-name f) files))))
    (sort (delete-dups files) #'string<)))

(defun qss-elisp-xref--origin (path)
  "Return (ORIGIN PACKAGE VERSION) of the file PATH."
  (let ((lisp-dir (expand-file-name (qss-elisp-xref--lisp-directory)))
        (elpa (and (boundp 'package-user-dir) (file-name-as-directory (expand-file-name package-user-dir)))))
    (cond ((string-prefix-p lisp-dir path) (list "emacs" nil nil))
          ((and elpa (string-prefix-p elpa path))
           (let ((dir (car (split-string (substring path (length elpa)) "/"))))
             (if (string-match "\\`\\(.*\\)-\\([0-9][0-9.]*\\)\\'" dir)
                 (list "elpa" (match-string 1 dir) (match-string 2 dir))
               (list "elpa" dir nil))))
          (t (list "user" nil nil)))))

;;; ------------------------------------------------------------------
;;; Reading one file

(defconst qss-elisp-xref--definers
  ;; HEAD: (KIND NAME-INDEX ARGLIST-INDEX DOC-INDEX)
  '((defun function 1 2 3) (defsubst function 1 2 3) (define-inline function 1 2 3)
    (cl-defun function 1 2 3) (cl-defsubst function 1 2 3) (cl-iter-defun function 1 2 3)
    (defmacro macro 1 2 3) (cl-defmacro macro 1 2 3)
    (defvar variable 1 nil 3) (defvar-local variable 1 nil 3) (defconst constant 1 nil 3)
    (defcustom custom 1 nil 3) (defface face 1 nil 3) (defgroup group 1 nil 3)
    (cl-defgeneric generic 1 2 3) (cl-defmethod method 1 nil nil)
    (cl-defstruct struct 1 nil 2) (defclass class 1 nil nil)
    (define-minor-mode mode 1 nil 2) (define-derived-mode mode 1 nil 4)
    (define-globalized-minor-mode mode 1 nil nil)
    (defalias alias 1 nil 3) (defvaralias alias 1 nil 3)
    (define-obsolete-function-alias alias 1 nil nil)
    (define-obsolete-variable-alias alias 1 nil nil)
    (define-error error 1 nil nil) (ert-deftest test 1 2 3)
    (define-widget widget 1 nil 3) (define-skeleton function 1 nil 2))
  "Forms that define something, and where their parts are.")

(defconst qss-elisp-xref--local-definers
  '((cl-flet . local-function) (cl-flet* . local-function) (cl-labels . local-function)
    (cl-macrolet . local-macro))
  "Forms that define local functions or macros in their first argument.")

(defconst qss-elisp-xref--binders
  '(let let* letrec dlet when-let when-let* if-let if-let* and-let* while-let
        cl-letf cl-letf* named-let pcase-let pcase-let*)
  "Forms whose first argument binds variables.")

(defconst qss-elisp-xref--lambdas '(lambda closure cl-function)
  "Forms with an argument list first.")

(defvar qss-elisp-xref--state nil
  "The collections of the file being read: a plist.")

(defun qss-elisp-xref--bare (x)
  "Return X without its position, if it is a symbol with position."
  (if (symbol-with-pos-p x) (bare-symbol x) x))

(defun qss-elisp-xref--symbolp (x)
  "Return non-nil if X is a symbol, with or without position."
  (or (symbol-with-pos-p x) (symbolp x)))

(defun qss-elisp-xref--name-of (x)
  "Return the symbol named by X: a symbol, \='symbol, #\='symbol, or (symbol ...)."
  (let ((x (qss-elisp-xref--bare x)))
    (cond ((symbolp x) x)
          ((and (consp x) (memq (qss-elisp-xref--bare (car x)) '(quote function)))
           (qss-elisp-xref--bare (cadr x)))
          ((consp x) (qss-elisp-xref--bare (car x))))))

(defun qss-elisp-xref--print (x)
  "Return X printed, symbols without positions.
Without print-quoted: the arglist (function sequence) stays as it is."
  (let ((print-symbols-bare t) (print-length nil) (print-level nil) (print-quoted nil))
    (prin1-to-string x)))

(defun qss-elisp-xref--add (key item)
  "Push ITEM onto the collection KEY of the state."
  (setq qss-elisp-xref--state
        (plist-put qss-elisp-xref--state key (cons item (plist-get qss-elisp-xref--state key)))))

(defun qss-elisp-xref--occurrence (sym context parent arg def)
  "Collect the symbol SYM (with position) in CONTEXT."
  (when (symbol-with-pos-p sym)
    (let ((bare (bare-symbol sym)))
      (unless (memq bare '(nil t))
        (qss-elisp-xref--add :occurrences
                             (list (symbol-name bare) context (symbol-with-pos-pos sym)
                                   parent arg def))))))

(defun qss-elisp-xref--string (s parent arg def &optional doc)
  "Collect the string S; DOC non-nil if it is a docstring (not stored as such)."
  (qss-elisp-xref--add :strings (list s parent arg def doc)))

(defun qss-elisp-xref--new-definition (name-x kind args doc parent &optional specializers)
  "Collect a definition and return its number in this file.
ARGS is the arglist, or :none for a definition without one."
  (let* ((n (1+ (length (plist-get qss-elisp-xref--state :definitions))))
         (name (qss-elisp-xref--name-of name-x))
         (pos (cond ((symbol-with-pos-p name-x) (symbol-with-pos-pos name-x))
                    ((consp name-x)
                     (let ((s (cl-find-if #'symbol-with-pos-p (flatten-tree name-x))))
                       (and s (symbol-with-pos-pos s)))))))
    (qss-elisp-xref--add :definitions
                         (list n (and name (symbol-name name)) (symbol-name kind)
                               specializers (cond ((eq args :none) nil)
                                                  (args (qss-elisp-xref--print args))
                                                  (t "()"))
                               doc pos parent))
    n))

(defun qss-elisp-xref--walk-args (list context parent arg def)
  "Collect the symbols of the argument LIST as CONTEXT."
  (dolist (a (if (proper-list-p list) list (flatten-tree list)))
    (cond ((qss-elisp-xref--symbolp a)
           (qss-elisp-xref--occurrence a (if (memq (qss-elisp-xref--bare a) '(&optional &rest &key &aux &body))
                                             'other context)
                                       parent arg def))
          ((consp a)                      ; (x default) or ((x y) specializer)
           (qss-elisp-xref--occurrence (car (flatten-tree (car a))) context parent arg def)
           (dolist (rest (cdr a)) (qss-elisp-xref--walk rest parent arg def nil))))))

(defun qss-elisp-xref--walk-list (list parent def quoted &optional from)
  "Walk the elements of LIST as arguments of PARENT, counting from FROM (1)."
  (let ((i (or from 1)) (l list))
    (while (consp l)
      (qss-elisp-xref--walk (car l) parent i def quoted)
      (setq l (cdr l) i (1+ i)))
    (when l (qss-elisp-xref--walk l parent i def quoted))))

(defun qss-elisp-xref--walk-definition (form head spec parent-def)
  "Collect the definition FORM with HEAD, of SPEC (KIND NAME ARGS DOC)."
  (pcase-let* ((`(,kind ,name-i ,args-i ,doc-i) spec)
               (name-x (nth name-i form))
               (args (if args-i (nth args-i form) :none))
               (specializers nil)
               (body-start (or doc-i (1+ (or args-i name-i)))))
    (when (eq head 'cl-defmethod)
      ;; (cl-defmethod NAME [QUALIFIERS...] ARGLIST [DOC] BODY)
      (let ((i 2))
        (while (and (< i (length form)) (not (listp (qss-elisp-xref--bare (nth i form)))))
          (setq i (1+ i))
          (when (stringp (nth i form)) (setq i (1+ i)))) ; :extra "name"
        (setq args (nth i form) args-i i doc-i (1+ i) body-start (1+ i)
              specializers (qss-elisp-xref--print
                            (cl-remove-if-not #'consp (if (listp args) args nil))))))
    (let* ((doc-x (and doc-i (nth doc-i form)))
           (doc (and (stringp doc-x) (or (> (length form) (1+ doc-i)) (memq kind '(variable constant custom face group alias generic))) doc-x))
           (def (qss-elisp-xref--new-definition name-x kind args doc parent-def specializers))
           (interactive nil))
      ;; the name, also quoted as in (defalias 'NAME ...)
      (let ((s (if (and (consp name-x) (memq (qss-elisp-xref--bare (car name-x)) '(quote function))
                        (consp (cdr name-x)) (symbol-with-pos-p (cadr name-x)))
                   (cadr name-x)
                 name-x)))
        (if (symbol-with-pos-p s)
            (qss-elisp-xref--occurrence s 'defining (symbol-name head) name-i def)
          (qss-elisp-xref--walk name-x (symbol-name head) name-i def nil)))
      ;; everything else, in order
      (let ((i 1) (l (cdr form)))
        (while (consp l)
          (let ((x (car l)))
            (cond ((= i name-i))
                  ((and args-i (= i args-i))
                   (qss-elisp-xref--walk-args x 'parameter (symbol-name head) i def))
                  ((and doc (= i doc-i) (eq x doc))
                   (qss-elisp-xref--string x (symbol-name head) i def t))
                  (t (when (and (>= i body-start) (consp x)
                                (eq (qss-elisp-xref--bare (car x)) 'interactive))
                       (setq interactive t))
                     (qss-elisp-xref--walk x (symbol-name head) i def nil))))
          (setq l (cdr l) i (1+ i))))
      (when interactive
        (qss-elisp-xref--add :interactive def)))))

(defun qss-elisp-xref--walk (x parent arg def quoted)
  "Collect what X contains; X is argument ARG of PARENT, in definition DEF.
QUOTED non-nil: inside ' or ` (data, except below , and ,@)."
  (cond
   ((symbol-with-pos-p x)
    (qss-elisp-xref--occurrence x (if quoted 'quoted 'other) parent arg def))
   ((stringp x) (qss-elisp-xref--string x parent arg def))
   ((vectorp x)
    (dotimes (i (length x)) (qss-elisp-xref--walk (aref x i) parent arg def t)))
   ((not (consp x)))
   ;; a dotted list such as (quote . edebug-read-quote) is data
   ((not (proper-list-p x))
    (while (consp x)
      (qss-elisp-xref--walk (car x) parent arg def t)
      (setq x (cdr x)))
    (qss-elisp-xref--walk x parent arg def t))
   (t
    (let* ((head-x (car x))
           (head (and (qss-elisp-xref--symbolp head-x) (qss-elisp-xref--bare head-x)))
           (name (and head (symbol-name head))))
      (cond
       ;; quote, function, backquote and its commas
       ((eq head 'quote)
        (qss-elisp-xref--walk (cadr x) parent arg def t))
       ((eq head 'function)
        (let ((f (cadr x)))
          (if (qss-elisp-xref--symbolp f)
              (qss-elisp-xref--occurrence f 'function parent arg def)
            (qss-elisp-xref--walk f parent arg def nil))))
       ((eq head '\`) (qss-elisp-xref--walk (cadr x) parent arg def t))
       ((memq head '(\, \,@)) (qss-elisp-xref--walk (cadr x) parent arg def nil))
       ;; data: every element is data
       (quoted
        (qss-elisp-xref--walk-list x nil def t 0))
       ;; definitions
       ((assq head qss-elisp-xref--definers)
        (qss-elisp-xref--occurrence head-x 'head parent arg def)
        (qss-elisp-xref--walk-definition x head (cdr (assq head qss-elisp-xref--definers)) def))
       ((assq head qss-elisp-xref--local-definers)
        (qss-elisp-xref--occurrence head-x 'head parent arg def)
        (dolist (binding (and (proper-list-p (cadr x)) (cadr x)))
          (if (and (consp binding) (qss-elisp-xref--symbolp (car binding)))
              (let ((local (qss-elisp-xref--new-definition
                            (car binding) (cdr (assq head qss-elisp-xref--local-definers))
                            (cadr binding) nil def)))
                (qss-elisp-xref--occurrence (car binding) 'defining name 1 local)
                (qss-elisp-xref--walk-args (cadr binding) 'parameter name 1 local)
                (qss-elisp-xref--walk-list (cddr binding) name local nil 2))
            (qss-elisp-xref--walk binding name 1 def nil)))
        (qss-elisp-xref--walk-list (cddr x) name def nil 2))
       ;; let and friends: the names in the first argument are bindings
       ((memq head qss-elisp-xref--binders)
        (qss-elisp-xref--occurrence head-x 'head parent arg def)
        ;; (named-let NAME BINDINGS BODY) has its bindings one later
        (let* ((named (and (eq head 'named-let) (qss-elisp-xref--symbolp (cadr x))))
               (bindings (if named (caddr x) (cadr x)))
               (body (if named (cdddr x) (cddr x))))
          (when named (qss-elisp-xref--occurrence (cadr x) 'defining name 1 def))
          (dolist (b (if (proper-list-p bindings) bindings nil))
            (cond ((qss-elisp-xref--symbolp b) (qss-elisp-xref--occurrence b 'binding name 1 def))
                  ((and (consp b) (qss-elisp-xref--symbolp (car b)))
                   (qss-elisp-xref--occurrence (car b) 'binding name 1 def)
                   (qss-elisp-xref--walk-list (cdr b) name def nil 1))
                  (t (qss-elisp-xref--walk b name 1 def nil))))
          (qss-elisp-xref--walk-list body name def nil (if named 3 2))))
       ((memq head qss-elisp-xref--lambdas)
        (qss-elisp-xref--occurrence head-x 'head parent arg def)
        (qss-elisp-xref--walk-args (cadr x) 'parameter name 1 def)
        (qss-elisp-xref--walk-list (cddr x) name def nil 2))
       ;; cond: the clauses are lists of expressions, not calls
       ((eq head 'cond)
        (qss-elisp-xref--occurrence head-x 'head parent arg def)
        (let ((i 1))
          (dolist (clause (if (proper-list-p (cdr x)) (cdr x) nil))
            (if (consp clause)
                (qss-elisp-xref--walk-list clause name def nil i)
              (qss-elisp-xref--walk clause name i def nil))
            (setq i (1+ i)))))
       ;; condition-case VAR BODY (CONDITION BODY...)...
       ((eq head 'condition-case)
        (qss-elisp-xref--occurrence head-x 'head parent arg def)
        (qss-elisp-xref--occurrence (cadr x) 'binding name 1 def)
        (qss-elisp-xref--walk (caddr x) name 2 def nil)
        (let ((i 3))
          (dolist (handler (and (proper-list-p (cdddr x)) (cdddr x)))
            (when (consp handler)
              (if (qss-elisp-xref--symbolp (car handler))
                  (qss-elisp-xref--occurrence (car handler) 'quoted name i def)
                (qss-elisp-xref--walk (car handler) name i def t))
              (qss-elisp-xref--walk-list (cdr handler) name def nil i))
            (setq i (1+ i)))))
       ;; a call, a macro, a special form: the head and its arguments
       (head
        (qss-elisp-xref--occurrence head-x 'head parent arg def)
        (qss-elisp-xref--walk-list (cdr x) name def nil 1))
       ;; ((lambda ...) args) and the like
       (t (qss-elisp-xref--walk head-x parent arg def nil)
          (qss-elisp-xref--walk-list (cdr x) nil def nil 1)))))))

;;; Positions of strings: the reader gives none, the text has them

(defun qss-elisp-xref--string-positions (start end)
  "Return the start positions of the string literals between START and END.
In the order of the text, which is the order the reader meets them."
  (let ((positions nil))
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
        (goto-char start)
        (let ((state nil))
          (while (< (point) end)
            (setq state (parse-partial-sexp (point) end nil nil state 'syntax-table))
            (cond ((nth 3 state)               ; entered a string
                   (let ((s (nth 8 state)))
                     (push s positions)
                     (goto-char s)
                     (condition-case nil (forward-sexp 1)
                       (error (goto-char end)))
                     (setq state nil)))
                  ((nth 4 state)               ; entered a comment: to its end
                   (setq state (parse-partial-sexp (point) end nil nil state 'syntax-table))
                   (setq state nil))
                  (t (goto-char end)))))))
    (nreverse positions)))

;;; Lines

(defun qss-elisp-xref--line-starts ()
  "Return a vector of the start positions of the lines of this buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((starts (list (point-min))))
      (while (search-forward "\n" nil t) (push (point) starts))
      (vconcat (nreverse starts)))))

(defun qss-elisp-xref--line-col (starts pos)
  "Return (LINE . COLUMN) of POS, given the line STARTS (binary search)."
  (let ((lo 0) (hi (1- (length starts))))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi 1) 2)))
        (if (<= (aref starts mid) pos) (setq lo mid) (setq hi (1- mid)))))
    (cons (1+ lo) (- pos (aref starts lo)))))

;;; ------------------------------------------------------------------
;;; Writing one file

(defun qss-elisp-xref--insert-rows (db table columns rows)
  "Insert ROWS (lists of values) into TABLE with COLUMNS, many per statement."
  (let ((width (length columns)))
    (while rows
      (let* ((chunk (seq-take rows 200))
             (sql (format "INSERT INTO %s (%s) VALUES %s" table
                          (mapconcat #'identity columns ", ")
                          (mapconcat (lambda (_) (concat "(" (mapconcat #'identity (make-list width "?") ",") ")"))
                                     chunk ","))))
        (sqlite-execute db sql (vconcat (apply #'append chunk)))
        (setq rows (nthcdr 200 rows))))))

(defun qss-elisp-xref--forget-file (db file-id)
  "Delete everything of FILE-ID from DB."
  (dolist (table '("occurrences" "definitions" "forms" "calls" "compiled"))
    (sqlite-execute db (format "DELETE FROM %s WHERE file_id = ?" table) (vector file-id)))
  (sqlite-execute db "DELETE FROM files WHERE id = ?" (vector file-id)))

(defvar qss-elisp-xref--walk-errors nil
  "The forms of the current file the walker failed on, as messages.")

(defun qss-elisp-xref--index-buffer (db file-id)
  "Read the current buffer (the file FILE-ID) and write what it contains to DB.
Return the number of forms."
  (let* ((starts (qss-elisp-xref--line-starts))
         (form-id (qss-elisp-xref--next-id db "forms"))
         (def-base (1- (qss-elisp-xref--next-id db "definitions")))
         (forms nil) (definitions nil) (occurrences nil) (n-defs 0)
         (symbols-with-pos-enabled t))
    (goto-char (point-min))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (catch 'done
        (while t
          (forward-comment (buffer-size))
          (when (eobp) (throw 'done nil))
          (let* ((start (point))
                 (form (condition-case nil (read-positioning-symbols (current-buffer))
                         (end-of-file (throw 'done nil))
                         (invalid-read-syntax (forward-line 1) 'qss-elisp-xref--unreadable)))
                 (end (point)))
            (unless (eq form 'qss-elisp-xref--unreadable)
              (let ((qss-elisp-xref--state nil))
                ;; definitions of this form are numbered after those before:
                ;; N-DEFS placeholders make `qss-elisp-xref--new-definition'
                ;; count on from there
                (setq qss-elisp-xref--state (list :definitions (make-list n-defs nil)))
                ;; a form the walker cannot take is kept, with what was
                ;; collected up to the error
                (condition-case err (qss-elisp-xref--walk form nil 0 nil nil)
                  (error (push (format "line %d: %s" (car (qss-elisp-xref--line-col starts start))
                                       (error-message-string err))
                               qss-elisp-xref--walk-errors)))
                (let* ((defs (nreverse (seq-take (plist-get qss-elisp-xref--state :definitions)
                                                 (- (length (plist-get qss-elisp-xref--state :definitions))
                                                    n-defs))))
                       (interactive (plist-get qss-elisp-xref--state :interactive))
                       (strings (nreverse (plist-get qss-elisp-xref--state :strings)))
                       (positions (qss-elisp-xref--string-positions start end))
                       (strings-ok (= (length strings) (length positions)))
                       (head (and (consp form) (qss-elisp-xref--symbolp (car form))
                                  (symbol-name (qss-elisp-xref--bare (car form))))))
                  (push (list form-id file-id head
                              (car (qss-elisp-xref--line-col starts start))
                              (car (qss-elisp-xref--line-col starts (max start (1- end))))
                              (buffer-substring-no-properties start end))
                        forms)
                  (dolist (d defs)
                    (pcase-let ((`(,n ,name ,kind ,spec ,args ,doc ,pos ,parent) d))
                      (push (list (+ def-base n) form-id file-id (and parent (+ def-base parent))
                                  name kind spec args (if (memq n interactive) 1 0) doc
                                  (car (qss-elisp-xref--line-col starts (or pos start))))
                            definitions)))
                  (dolist (o (plist-get qss-elisp-xref--state :occurrences))
                    (pcase-let ((`(,value ,context ,_pos ,parent ,arg ,def) o)
                                (lc (qss-elisp-xref--line-col starts (car (cddr o)))))
                      (push (list value (symbol-name context) file-id form-id (and def (+ def-base def))
                                  (car lc) (cdr lc) parent arg)
                            occurrences)))
                  ;; strings: positions from the text, in the same order
                  (cl-loop for s in strings
                           ;; walk the positions along: nth would be
                           ;; quadratic (ja-dic.el has 148 000 strings)
                           for rest = (and strings-ok positions) then (cdr rest)
                           for pos = (car rest)
                           unless (nth 4 s)       ; a docstring
                           do (let ((lc (qss-elisp-xref--line-col starts (or pos start))))
                                (push (list (car s) "string" file-id form-id
                                            (and (nth 3 s) (+ def-base (nth 3 s)))
                                            (car lc) (and pos (cdr lc)) (nth 1 s) (nth 2 s))
                                      occurrences)))
                  (setq n-defs (+ n-defs (length defs)))
                  (setq form-id (1+ form-id)))))))))
    (setq forms (nreverse forms))             ; nreverse: use the result, not the old head
    (qss-elisp-xref--insert-rows db "forms" '("id" "file_id" "head" "start_line" "end_line" "text")
                                 forms)
    (qss-elisp-xref--insert-rows db "definitions"
                                 '("id" "form_id" "file_id" "parent_id" "name" "kind" "specializers"
                                   "arglist" "interactive" "doc" "line")
                                 (nreverse definitions))
    (qss-elisp-xref--insert-rows db "occurrences"
                                 '("value" "context" "file_id" "form_id" "definition_id"
                                   "line" "col" "parent_head" "arg_index")
                                 (nreverse occurrences))
    (length forms)))

(defun qss-elisp-xref--index-file (db path)
  "Index the file PATH into DB, unless it is unchanged.  Return t if read."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((sha1 (secure-hash 'sha1 (encode-coding-string (buffer-string) 'utf-8-emacs t)))
           ;; not the buffer: hashing it encodes it in the file's coding
           ;; system, and Emacs asks for another if that fails (ARRAY30.el)
           (old (car (sqlite-select db "SELECT id, sha1 FROM files WHERE path = ?" (vector path)))))
      (if (equal (nth 1 old) sha1)
          nil
        (with-sqlite-transaction db
          (when old (qss-elisp-xref--forget-file db (car old)))
          (let* ((id (qss-elisp-xref--next-id db "files"))
                 (origin (qss-elisp-xref--origin path))
                 (library (replace-regexp-in-string "\\.el\\(?:\\.gz\\)?\\'" "" (file-name-nondirectory path)))
                 (lexical (save-excursion
                            (goto-char (point-min))
                            (and (re-search-forward "lexical-binding: *t" (line-end-position 2) t) 1)))
                 (commentary (ignore-errors (lm-commentary)))
                 (version (or (nth 2 origin) (ignore-errors (lm-header "Version"))))
                 (error nil) (n 0))
            (sqlite-execute db "INSERT INTO files (id, path, library, origin, package, package_version,
                                  lexical, commentary, mtime, sha1) VALUES (?,?,?,?,?,?,?,?,?,?)"
                            (vector id path library (nth 0 origin) (nth 1 origin) version
                                    (or lexical 0) commentary
                                    (format-time-string "%Y-%m-%d %H:%M:%S"
                                                        (file-attribute-modification-time (file-attributes path)))
                                    sha1))
            (let ((qss-elisp-xref--walk-errors nil))
              (condition-case err
                  (setq n (qss-elisp-xref--index-buffer db id))
                (error (setq error (error-message-string err))))
              (when qss-elisp-xref--walk-errors
                (setq error (string-join (cons error (reverse qss-elisp-xref--walk-errors)) "\n"))
                (setq error (string-trim error))))
            (sqlite-execute db "UPDATE files SET forms = ?, error = ? WHERE id = ?" (vector n error id))))
        t))))

;;; The calls the byte compiler sees

(defun qss-elisp-xref--compile-calls (path)
  "Compile the file PATH into a directory thrown away; return its calls.
The result is a list of (CALLER CALLEE) names, as strings; the byte
compiler's call tree (`byte-compile-generate-call-tree')."
  (let* ((tmp (make-temp-file "qss-xref-elc-" t))
         (byte-compile-generate-call-tree t)
         (byte-compile-call-tree nil)
         (byte-compile-dest-file-function
          (lambda (_) (expand-file-name "out.elc" tmp)))
         (byte-compile-log-warning-function #'ignore)
         (inhibit-message t)
         (calls nil)
         (result nil))
    (advice-add 'display-call-tree :override #'ignore)
    (unwind-protect
        (progn
          (setq result (byte-compile-file path))
          (pcase-dolist (`(,caller ,_callers ,callees) byte-compile-call-tree)
            (dolist (callee callees)
              (when (and (symbolp caller) (symbolp callee))
                (push (list (and caller (symbol-name caller)) (symbol-name callee)) calls)))))
      (advice-remove 'display-call-tree #'ignore)
      (delete-directory tmp t))
    (if (eq result 'no-byte-compile)
        'no-byte-compile
      (delete-dups calls))))

(defun qss-elisp-xref-calls-run (&optional file)
  "Add the calls of the files in the SQLite FILE not compiled yet.
FILE defaults to `qss-elisp-xref-database'.  Compiling loads what the
files require, so this belongs in an Emacs of its own, after
`qss-elisp-xref-run' (see `qss-elisp-xref-index')."
  (let* ((db (qss-elisp-xref--open (or file (qss-elisp-xref-database-file))))
         (todo (sqlite-select db "SELECT id, path FROM files
                                  WHERE id NOT IN (SELECT file_id FROM compiled) ORDER BY path"))
         (i 0) (start (float-time)))
    (unwind-protect
        (pcase-dolist (`(,id ,path) todo)
          (setq i (1+ i))
          (let* ((error nil)
                 (calls (condition-case err (qss-elisp-xref--compile-calls path)
                          (error (setq error (error-message-string err)) nil))))
            (when (eq calls 'no-byte-compile)
              (setq calls nil error "no-byte-compile"))
            (with-sqlite-transaction db
              (sqlite-execute db "DELETE FROM calls WHERE file_id = ?" (vector id))
              (qss-elisp-xref--insert-rows db "calls" '("caller" "callee" "file_id")
                                           (mapcar (lambda (c) (append c (list id))) calls))
              (sqlite-execute db "INSERT OR REPLACE INTO compiled VALUES (?, ?)" (vector id error))))
          (when (zerop (% i 50))
            (message "calls: %d/%d files, %.0f s" i (length todo) (- (float-time) start))))
      (message "Calls done: %d files compiled, %.0f s" i (- (float-time) start))
      (sqlite-close db))))

(defun qss-elisp-xref-calls-batch ()
  "Entry for emacs --batch: `qss-elisp-xref-calls-run'.
The variable `qss-elisp-xref-database' names the file (--eval to set it).
The installed packages are activated first, so that the files of a
package find the packages they require."
  (when (require 'package nil t) (package-initialize))
  (let ((inhibit-interaction t))         ; see `qss-elisp-xref-batch'
    (qss-elisp-xref-calls-run)))

;;; After all files: what C defines, and what the heads are

(defun qss-elisp-xref--finish (db)
  "Add the primitives and special forms of C to DB.
Then mark the heads that are macro uses and special forms."
  (with-sqlite-transaction db
    (sqlite-execute db "DELETE FROM definitions WHERE file_id IS NULL")
    (let ((id (qss-elisp-xref--next-id db "definitions")) (rows nil))
      (mapatoms (lambda (s)
                  (when (and (fboundp s) (subrp (symbol-function s))
                             (not (and (fboundp 'subr-native-elisp-p)
                                       (subr-native-elisp-p (symbol-function s)))))
                    (push (list id (symbol-name s) (if (special-form-p s) "special-form" "primitive")
                                (qss-elisp-xref--subr-arglist s)
                                (ignore-errors (car (split-string (or (documentation s t) "") "\n"))))
                          rows)
                    (setq id (1+ id)))))
      (qss-elisp-xref--insert-rows db "definitions" '("id" "name" "kind" "arglist" "doc") rows))
    (sqlite-execute db "UPDATE occurrences SET context = 'head' WHERE context IN ('macro-use', 'special-form')")
    (sqlite-execute db "UPDATE occurrences SET context = 'special-form'
                        WHERE context = 'head' AND value IN
                          (SELECT name FROM definitions WHERE kind = 'special-form')")
    (sqlite-execute db "UPDATE occurrences SET context = 'macro-use'
                        WHERE context = 'head' AND value IN
                          (SELECT name FROM definitions WHERE kind = 'macro')")
    ;; macros of files not indexed: the running Emacs knows them
    (let ((macros (cl-loop for (name) in (sqlite-select db "SELECT DISTINCT value FROM occurrences
                                                           WHERE context = 'head'")
                           when (macrop (intern-soft name)) collect (list name))))
      (sqlite-execute db "CREATE TEMP TABLE IF NOT EXISTS known_macros (name TEXT PRIMARY KEY)")
      (sqlite-execute db "DELETE FROM known_macros")
      (qss-elisp-xref--insert-rows db "known_macros" '("name") macros)
      (sqlite-execute db "UPDATE occurrences SET context = 'macro-use'
                          WHERE context = 'head' AND value IN (SELECT name FROM known_macros)"))
    (qss-elisp-xref--summaries db)
    (qss-elisp-xref--categories db)
    (dolist (kv `(("format" . ,(number-to-string qss-elisp-xref-format))
                  ("emacs-version" . ,emacs-version)
                  ("indexed-at" . ,(format-time-string "%Y-%m-%d %H:%M:%S"))))
      (sqlite-execute db "INSERT OR REPLACE INTO meta VALUES (?, ?)" (vector (car kv) (cdr kv))))))

;;; ------------------------------------------------------------------
;;; Summaries and categories: what there is, the most used first

(defun qss-elisp-xref--subr-arglist (subr)
  "The arglist of the primitive SUBR as text, from its documentation, or nil."
  (let ((args (ignore-errors (help-function-arglist subr t))))
    (and (listp args) (qss-elisp-xref--print args))))

(defconst qss-elisp-xref--callable-kinds
  "('function', 'alias', 'macro', 'primitive', 'special-form', 'generic', 'command')"
  "The kinds of definitions one calls, as SQL.")

(defun qss-elisp-xref--summaries (db)
  "Fill the table summaries of DB: one row per callable name, with its uses.
Uses count calls, #' and macro uses; files the files they are in."
  (sqlite-execute db "DELETE FROM summaries")
  (sqlite-execute db (format "
    INSERT INTO summaries (name, kind, arglist, summary, uses, files)
    SELECT d.name, min(d.kind), max(d.arglist),
           max(CASE WHEN d.doc IS NULL THEN NULL
                    ELSE substr(d.doc, 1, instr(d.doc || char(10), char(10)) - 1) END),
           coalesce(u.uses, 0), coalesce(u.files, 0)
      FROM definitions d
      LEFT JOIN (SELECT value, count(*) AS uses, count(DISTINCT file_id) AS files
                   FROM occurrences
                  WHERE context IN ('head', 'function', 'macro-use', 'special-form')
                  GROUP BY value) u ON u.value = d.name
     WHERE d.kind IN %s AND d.parent_id IS NULL
     GROUP BY d.name" qss-elisp-xref--callable-kinds)))

(defconst qss-elisp-xref-category-prefixes
  '(("string-" . "string") ("seq-" . "sequence") ("file-name-" . "file-name")
    ("buffer-" . "buffer") ("hash-table-" . "hash-table") ("regexp-" . "regexp")
    ("alist-" . "alist") ("overlay-" . "overlay") ("process-" . "process")
    ("keymap-" . "keymaps") ("symbol-" . "symbol") ("number-" . "number")
    ("vector-" . "vector") ("file-" . "file"))
  "Name prefixes that put a function into a category (the first match).")

(defconst qss-elisp-xref-category-parameters
  '((string . "string") (str . "string") (s . "string")
    (seq . "sequence") (sequence . "sequence")
    (list . "list") (lst . "list") (alist . "alist")
    (vector . "vector") (vec . "vector")
    (buffer . "buffer") (buf . "buffer") (buffer-or-name . "buffer")
    (file . "file") (filename . "file-name") (file-name . "file-name")
    (regexp . "regexp") (regex . "regexp")
    (symbol . "symbol") (sym . "symbol") (keymap . "keymaps")
    (process . "process") (proc . "process") (overlay . "overlay")
    (number . "number") (hash-table . "hash-table"))
  "Parameter names that put a function into a category.")

(defconst qss-elisp-xref-category-types
  '((string . "string") (sequence . "sequence") (list . "list") (cons . "list")
    (vector . "vector") (bool-vector . "vector") (array . "sequence")
    (number . "number") (integer . "number") (fixnum . "number") (float . "number")
    (natnum . "number") (buffer . "buffer") (symbol . "symbol") (keyword . "symbol")
    (hash-table . "hash-table") (process . "process") (overlay . "overlay"))
  "Types in the type specifiers of the native compiler, and their category.")

(defun qss-elisp-xref--shortdoc-members ()
  "Alist (FUNCTION-NAME . GROUP) of the shortdoc groups of this Emacs."
  (and (require 'shortdoc nil t) (boundp 'shortdoc--groups)
       (cl-loop for (group . entries) in (symbol-value 'shortdoc--groups)
                append (cl-loop for e in entries
                                when (and (consp e) (symbolp (car e)) (not (keywordp (car e))))
                                collect (cons (symbol-name (car e)) (symbol-name group))))))

(defun qss-elisp-xref--type-members ()
  "Alist (FUNCTION-NAME . CATEGORY) from the types the native compiler knows."
  (when (or (require 'comp-common nil t) (require 'comp nil t))
    (and (boundp 'comp-known-type-specifiers)
         (cl-loop for (f type) in (symbol-value 'comp-known-type-specifiers)
                  append (let ((cats nil))
                           (cl-labels ((walk (x) (cond ((consp x) (walk (car x)) (walk (cdr x)))
                                                       ((symbolp x)
                                                        (let ((c (alist-get x qss-elisp-xref-category-types)))
                                                          (when c (cl-pushnew c cats :test #'equal)))))))
                             (walk type))
                           (mapcar (lambda (c) (cons (symbol-name f) c)) cats))))))

(defun qss-elisp-xref--arglist-symbols (arglist)
  "The parameter names in the printed ARGLIST, as symbols."
  (let ((args (ignore-errors (car (read-from-string arglist)))))
    (cl-loop for a in (and (listp args) args)
             for name = (if (consp a) (car a) a)   ; (x type) of cl-defmethod
             when (and (symbolp name) (not (memq name '(&optional &rest &key &body))))
             collect (intern (downcase (symbol-name name))))))

(defun qss-elisp-xref--categories (db)
  "Fill the table categories of DB from shortdoc, types, prefixes and parameters.
One row per name and category; sources says what put it there."
  (let ((found (make-hash-table :test #'equal)))
    (cl-flet ((add (name cat source)
                (let ((key (cons name cat)))
                  (cl-pushnew source (gethash key found) :test #'equal))))
      (pcase-dolist (`(,name . ,cat) (qss-elisp-xref--shortdoc-members)) (add name cat "shortdoc"))
      (pcase-dolist (`(,name . ,cat) (qss-elisp-xref--type-members)) (add name cat "type"))
      (pcase-dolist (`(,name ,arglist) (sqlite-select db "SELECT name, arglist FROM summaries"))
        (let ((prefix (cl-find-if (lambda (p) (string-prefix-p (car p) name))
                                  qss-elisp-xref-category-prefixes)))
          (when prefix (add name (cdr prefix) "prefix")))
        (when arglist
          (dolist (p (qss-elisp-xref--arglist-symbols arglist))
            (let ((cat (alist-get p qss-elisp-xref-category-parameters)))
              (when cat (add name cat "param")))))))
    (sqlite-execute db "DELETE FROM categories")
    (let (rows)
      (maphash (lambda (key sources)
                 (push (list (car key) (cdr key) (string-join (sort sources #'string<) ",")) rows))
               found)
      ;; only names this database knows
      (qss-elisp-xref--insert-rows db "categories" '("name" "category" "sources") rows)
      (sqlite-execute db "DELETE FROM categories WHERE name NOT IN (SELECT name FROM summaries)"))))

;;; ------------------------------------------------------------------
;;; Running

(defun qss-elisp-xref-run (&optional dirs file)
  "Index the Emacs Lisp files under DIRS into the SQLite FILE.
Defaults: `qss-elisp-xref-directories', `qss-elisp-xref-database'.
Only changed files are read again; files gone are dropped.  Meant for
an Emacs of its own (see `qss-elisp-xref-index'); it takes a while."
  (let* ((dirs (or dirs (qss-elisp-xref--directories)))
         (files (qss-elisp-xref--files dirs))
         (db (qss-elisp-xref--open (or file (qss-elisp-xref-database-file))))
         (read 0) (i 0) (start (float-time)))
    (unwind-protect
        (progn
          ;; files gone
          (let ((known (make-hash-table :test #'equal)))
            (dolist (f files) (puthash f t known))
            (dolist (row (sqlite-select db "SELECT id, path FROM files"))
              (unless (gethash (nth 1 row) known)
                (with-sqlite-transaction db (qss-elisp-xref--forget-file db (car row))))))
          (dolist (f files)
            (setq i (1+ i))
            (when (condition-case err (qss-elisp-xref--index-file db f)
                    (error (message "%s: %s" f (error-message-string err)) nil))
              (setq read (1+ read)))
            (when (zerop (% i 100))
              (message "%d/%d files, %d read, %.0f s" i (length files) read (- (float-time) start))))
          (qss-elisp-xref--finish db)
          (message "Done: %d files, %d read again, %.0f s" (length files) read (- (float-time) start)))
      (sqlite-close db))))

(defun qss-elisp-xref-batch ()
  "Entry for emacs --batch: the remaining arguments are the directories.
  emacs --batch -L QSS -l qss-elisp-xref -f qss-elisp-xref-batch [DIR...]
The variable `qss-elisp-xref-database' names the file (--eval to set it)."
  (let ((dirs (mapcar #'expand-file-name command-line-args-left))
        ;; a question would wait for ever: make it an error of that file
        (inhibit-interaction t))
    (setq command-line-args-left nil)
    (qss-elisp-xref-run dirs)))

;;;###autoload
(defun qss-elisp-xref-index (&optional dirs)
  "Index the Emacs Lisp files into `qss-elisp-xref-database', in the background.
DIRS default to `qss-elisp-xref-directories' (with a prefix argument:
ask for one directory).  An Emacs of its own does the work; its
messages are in the buffer *qss-elisp-xref*."
  (interactive (list (and current-prefix-arg (list (read-directory-name "Index directory: ")))))
  (let* ((dirs (or dirs (qss-elisp-xref--directories)))
         (here (file-name-directory (locate-library "qss-elisp-xref")))
         (buf (get-buffer-create "*qss-elisp-xref*")))
    (with-current-buffer buf (erase-buffer))
    (cl-flet ((step (fn &rest args)
                ;; each step in an Emacs of its own: compiling loads
                ;; what the files require
                (apply #'start-process "qss-elisp-xref" buf
                       (expand-file-name invocation-name invocation-directory)
                       "--batch" "-L" here "-l" "qss-elisp-xref"
                       "--eval" (format "(setq qss-elisp-xref-database %S package-user-dir %S)"
                                        (qss-elisp-xref-database-file)
                                        (and (boundp 'package-user-dir) package-user-dir))
                       "-f" fn args)))
      (set-process-sentinel
       (apply #'step "qss-elisp-xref-batch" (mapcar #'expand-file-name dirs))
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (if (zerop (process-exit-status proc))
               (set-process-sentinel
                (step "qss-elisp-xref-calls-batch")
                (lambda (p _e)
                  (when (memq (process-status p) '(exit signal))
                    (message "qss-elisp-xref: %s" (if (zerop (process-exit-status p))
                                                      "done" "the calls step failed")))))
             (message "qss-elisp-xref: reading failed; see *qss-elisp-xref*"))))))
    (display-buffer buf)
    (message "Indexing %s in the background; see *qss-elisp-xref*"
             (mapconcat #'abbreviate-file-name dirs ", "))))

(provide 'qss-elisp-xref)
;;; qss-elisp-xref.el ends here
