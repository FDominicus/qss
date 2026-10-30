;;; qss-odbc.el --- ODBC databases, through a small C module  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; Any database with an ODBC driver: PostgreSQL, MariaDB, SQL Server,
;; Access (Windows), SQLite ...  Emacs Lisp has no FFI, so the calls
;; into ODBC live in a dynamic module, module/qss-odbc-module.c.  It
;; is built once with M-x qss-odbc-build-module (needs a C compiler
;; and the ODBC headers, on Debian/Ubuntu the package unixodbc-dev).
;;
;;   (setq con (qss-odbc-connect "test"))         ; a DSN, or
;;   (qss-odbc-connect "Driver={PostgreSQL Unicode};Server=...")
;;   (qss-odbc-query con "SELECT id, name FROM person WHERE id > ?" 10)
;;   => (("id" "name") (11 "Müller") (12 "Ærø"))      ; with :columns
;;   (qss-odbc-do-query ((id name) con "SELECT id, name FROM person")
;;     (message "%s %s" id name))
;;   (qss-odbc-execute con "UPDATE person SET name = ? WHERE id = ?" "X" 11)
;;   => 1
;;   (qss-odbc-tables con)
;;   (qss-odbc-disconnect con)
;;
;; Values from the database:
;;
;;   NULL                       nil
;;   integers                   integers (bignums if need be)
;;   REAL, FLOAT, DOUBLE        floats
;;   NUMERIC(p,s), DECIMAL      integers scaled by 10^s: 1094.80 in a
;;                              NUMERIC(10,2) is 109480; the scale is in
;;                              the column (`qss-odbc-column-scale').
;;                              `qss-odbc-decimal-string' shows it.
;;   BIT, BOOLEAN               t or nil
;;   DATE                       "2026-10-30"
;;   TIME                       "09:56:00"
;;   TIMESTAMP                  "2026-10-30 09:56:00" (".123456" if
;;                              there is a fraction); wall clock time,
;;                              no zone, as the database stores it.
;;                              `qss-odbc-parse-time' makes a Lisp time.
;;   binary                     unibyte strings
;;   text, everything else      strings
;;
;; Parameters (the `?' in the SQL):
;;
;;   nil                        NULL
;;   t                          TRUE (BIT); false is (qss-odbc-bool nil)
;;   integer, float, string     as such; integers beyond 64 bit as NUMERIC
;;   (qss-odbc-decimal 109480 2)            NUMERIC 1094.80
;;   (qss-odbc-binary "\377\0")             binary
;;   (qss-odbc-date "2026-10-30")           DATE
;;   (qss-odbc-timestamp "2026-10-30 09:56") TIMESTAMP (or a Lisp time)
;;
;; Errors are signalled as `qss-odbc-error' with the data
;; (MESSAGE SQLSTATE NATIVE-ERROR DIAGNOSTICS); see
;; `qss-odbc-error-sqlstate'.
;;
;; Connections start with autocommit on: every statement is its own
;; transaction unless `qss-odbc-with-transaction' says otherwise.
;; (plain-odbc started with it off; readers then held locks.)

;;; Code:

(require 'cl-lib)
(require 'time-date)                   ; make-decoded-time

(define-error 'qss-odbc-error "ODBC error")

;;; ------------------------------------------------------------------
;;; The module

(defconst qss-odbc--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of qss-odbc.el.")

(defcustom qss-odbc-module-directory
  (expand-file-name "module" qss-odbc--directory)
  "Directory with qss-odbc-module.c, its Makefile and the built module."
  :type 'directory :group 'qss)

(defcustom qss-odbc-make-arguments nil
  "Extra arguments for make when building the module.
For example (\"EMACS_INCLUDE=/home/me/src/emacs/src\") when
emacs-module.h is not installed, or (\"ODBC=iodbc\")."
  :type '(repeat string) :group 'qss)

(defun qss-odbc--module-file ()
  "The module file for this system."
  (expand-file-name (concat "qss-odbc-module" module-file-suffix)
                    qss-odbc-module-directory))

(declare-function qss-odbc--connect "qss-odbc-module")
(declare-function qss-odbc--disconnect "qss-odbc-module")
(declare-function qss-odbc--connected-p "qss-odbc-module")
(declare-function qss-odbc--commit "qss-odbc-module")
(declare-function qss-odbc--rollback "qss-odbc-module")
(declare-function qss-odbc--set-autocommit "qss-odbc-module")
(declare-function qss-odbc--set-sql-utf8 "qss-odbc-module")
(declare-function qss-odbc--get-info "qss-odbc-module")
(declare-function qss-odbc--exec "qss-odbc-module")
(declare-function qss-odbc--catalog "qss-odbc-module")
(declare-function qss-odbc--columns "qss-odbc-module")
(declare-function qss-odbc--fetch "qss-odbc-module")
(declare-function qss-odbc--row-count "qss-odbc-module")
(declare-function qss-odbc--more-results "qss-odbc-module")
(declare-function qss-odbc--close "qss-odbc-module")

;;;###autoload
(defun qss-odbc-build-module ()
  "Build the ODBC module with make and load it.
The output goes to the buffer *qss-odbc build*."
  (interactive)
  (unless module-file-suffix
    (user-error "This Emacs was built without module support (--with-modules)"))
  (let ((buf (get-buffer-create "*qss-odbc build*"))
        (default-directory (file-name-as-directory qss-odbc-module-directory)))
    (with-current-buffer buf (erase-buffer))
    (message "Building the ODBC module in %s ..." default-directory)
    (let ((status (apply #'call-process "make" nil buf t
                         (append qss-odbc-make-arguments
                                 (when (eq system-type 'windows-nt)
                                   '("OS=Windows_NT"))))))
      (unless (eql status 0)
        (display-buffer buf)
        (error "Building the ODBC module failed, see *qss-odbc build*"))))
  (qss-odbc--load-module t)
  (message "ODBC module built and loaded"))

(defun qss-odbc--load-module (&optional quiet)
  "Load the module unless loaded; offer to build it if missing.
With QUIET, do not ask."
  (unless (featurep 'qss-odbc-module)
    (let ((file (qss-odbc--module-file)))
      (unless (file-exists-p file)
        (if (and (not quiet) (not noninteractive)
                 (y-or-n-p "The ODBC module is not built yet.  Build it now? "))
            (qss-odbc-build-module)
          (error "ODBC module missing: %s (M-x qss-odbc-build-module)" file)))
      (unless (featurep 'qss-odbc-module)
        (module-load file)))))

(defun qss-odbc-available-p ()
  "Return t if the ODBC module is built or loaded."
  (or (featurep 'qss-odbc-module)
      (and module-file-suffix (file-exists-p (qss-odbc--module-file)) t)))

;;; ------------------------------------------------------------------
;;; Errors

(defun qss-odbc-error-message (err)
  "The message of the `qss-odbc-error' ERR (as caught by `condition-case')."
  (nth 1 err))

(defun qss-odbc-error-sqlstate (err)
  "The SQLSTATE of the `qss-odbc-error' ERR, e.g. \"42P01\", or nil."
  (nth 2 err))

(defun qss-odbc-error-diagnostics (err)
  "All diagnostic records of ERR: a list of (SQLSTATE NATIVE MESSAGE)."
  (nth 4 err))

;;; ------------------------------------------------------------------
;;; Connections

(cl-defstruct (qss-odbc-connection (:constructor qss-odbc--make-connection)
                                   (:copier nil))
  "An open ODBC connection."
  handle            ; the module's object
  string            ; connection string, without the password
  dbms              ; e.g. "PostgreSQL", "SQLite", "ACCESS"
  driver            ; file name of the driver
  quote             ; identifier quote character, e.g. "\""
  utf-8)            ; t: ANSI driver, text as UTF-8 bytes

(cl-defmethod cl-print-object ((con qss-odbc-connection) stream)
  (princ (format "#<qss-odbc %s%s %s%s>" (qss-odbc-connection-dbms con)
                 (if (qss-odbc-connection-utf-8 con) " (ANSI, UTF-8)" "")
                 (qss-odbc-connection-string con)
                 (if (qss-odbc--connected-p (qss-odbc-connection-handle con))
                     "" " closed"))
         stream))

(defun qss-odbc--connection-string (dsn)
  "DSN as a connection string: a plain name becomes DSN=NAME."
  (if (string-search "=" dsn) dsn (concat "DSN=" dsn)))

(defun qss-odbc--hide-password (string)
  "STRING without the value of PWD= or Password=."
  (replace-regexp-in-string "\\(\\(?:PWD\\|Password\\)=\\)[^;]*" "\\1***"
                            string t nil nil))

;;;###autoload
(defun qss-odbc-connect (dsn &rest options)
  "Connect to DSN: a data source name or a full connection string.
OPTIONS: :user, :password (added to the string), :autocommit (default t),
:utf-8 (t for an ANSI driver that expects UTF-8; by default after
`qss-odbc-ansi-utf-8-drivers').
Return a `qss-odbc-connection'."
  (qss-odbc--load-module)
  (let* ((string (concat (qss-odbc--connection-string dsn)
                         (when-let* ((u (plist-get options :user)))
                           (format ";UID=%s" u))
                         (when-let* ((p (plist-get options :password)))
                           (format ";PWD=%s" p))))
         (autocommit (if (plist-member options :autocommit)
                         (plist-get options :autocommit)
                       t))
         (handle (qss-odbc--connect string autocommit))
         (con (qss-odbc--make-connection
               :handle handle
               :string (qss-odbc--hide-password string)
               :dbms (qss-odbc--get-info handle 'dbms-name)
               :driver (qss-odbc--get-info handle 'driver-name)
               :quote (qss-odbc--get-info handle 'identifier-quote))))
    (when (if (plist-member options :utf-8)
              (plist-get options :utf-8)
            (qss-odbc--ansi-utf-8-driver-p (qss-odbc-connection-driver con)))
      (qss-odbc--set-sql-utf8 handle t)
      (setf (qss-odbc-connection-utf-8 con) t))
    con))

(defcustom qss-odbc-ansi-utf-8-drivers '("sqlite" "psqlodbca")
  "Regexps for drivers that are ANSI drivers expecting UTF-8.
With them, SQL text, text parameters and character columns go as UTF-8
bytes: such a driver rejects UTF-16 parameters (psqlODBC ANSI: HYC00
\"Unrecognized C_parameter type\"), and through the W functions unixODBC
turns the text into Latin-1 (SQLite).  Matched against the file name of
the driver; `qss-odbc-connect' with :utf-8 t or nil decides for itself."
  :type '(repeat regexp) :group 'qss)

(defun qss-odbc--ansi-utf-8-driver-p (driver)
  "Return non-nil if DRIVER (a file name) is in `qss-odbc-ansi-utf-8-drivers'."
  (cl-some (lambda (re) (string-match-p re driver)) qss-odbc-ansi-utf-8-drivers))

(defun qss-odbc-disconnect (con)
  "Close the connection CON and all its statements."
  (qss-odbc--disconnect (qss-odbc-connection-handle con)))

(defun qss-odbc-connected-p (con)
  "Return t if CON is open."
  (and (qss-odbc-connection-p con)
       (qss-odbc--connected-p (qss-odbc-connection-handle con))))

(defun qss-odbc-commit (con)
  "Commit the transaction on CON."
  (qss-odbc--commit (qss-odbc-connection-handle con)))

(defun qss-odbc-rollback (con)
  "Roll back the transaction on CON."
  (qss-odbc--rollback (qss-odbc-connection-handle con)))

(defmacro qss-odbc-with-transaction (con &rest body)
  "Run BODY in one transaction on CON: commit at the end, roll back on
a non-local exit."
  (declare (indent 1) (debug (form body)))
  (let ((c (make-symbol "con")) (h (make-symbol "handle")) (ok (make-symbol "ok")))
    `(let* ((,c ,con) (,h (qss-odbc-connection-handle ,c)) (,ok nil))
       (qss-odbc--set-autocommit ,h nil)
       (unwind-protect
           (prog1 (progn ,@body)
             (qss-odbc--commit ,h)
             (setq ,ok t))
         (unless ,ok (qss-odbc--rollback ,h))
         (qss-odbc--set-autocommit ,h t)))))

;;; ------------------------------------------------------------------
;;; Parameters

(defun qss-odbc-decimal (n scale)
  "The parameter for the exact number N / 10^SCALE (NUMERIC)."
  (vector 'decimal (qss-odbc-decimal-string n scale)
          (max 1 (length (number-to-string (abs n)))) scale))

(defun qss-odbc-binary (bytes)
  "The parameter for the binary data BYTES (a unibyte string)."
  (vector 'binary (if (multibyte-string-p bytes)
                      (encode-coding-string bytes 'raw-text)
                    bytes)))

(defun qss-odbc-bool (value)
  "The parameter TRUE or FALSE after VALUE (nil is FALSE, not NULL)."
  (vector 'bit (if value 1 0)))

(defun qss-odbc--decode (time)
  "TIME (a string \"2026-10-30 09:56:00\" or a Lisp time) as decoded time.
A Lisp time is taken in UTC: the database gets the wall clock."
  (if (stringp time)
      (qss-odbc-parse-time time t)
    (decode-time time t)))

(defun qss-odbc-date (date)
  "The parameter for DATE, \"2026-10-30\" or a Lisp time."
  (let ((d (qss-odbc--decode date)))
    (vector 'date (decoded-time-year d) (decoded-time-month d)
            (decoded-time-day d))))

(defun qss-odbc-timestamp (time)
  "The parameter for TIME, \"2026-10-30 09:56:00[.ffffff]\" or a Lisp time."
  (let* ((d (qss-odbc--decode time))
         (sec (decoded-time-second d))
         (whole (if (integerp sec) sec (floor (time-convert sec 'integer))))
         (frac (if (integerp sec) 0
                 (round (* 1e9 (- (float-time sec) whole))))))
    (vector 'timestamp (decoded-time-year d) (decoded-time-month d)
            (decoded-time-day d) (decoded-time-hour d)
            (decoded-time-minute d) whole frac)))

(defun qss-odbc--param (v)
  "The value V as the module expects it."
  (cond ((eq v t) [bit 1])
        ((and (integerp v)
              (or (> v 9223372036854775807) (< v -9223372036854775808)))
         ;; Beyond 64 bit: exact NUMERIC text
         (qss-odbc-decimal v 0))
        ((and (stringp v) (not (multibyte-string-p v))
              (string-match-p "[\200-\377]" v))
         ;; A unibyte string with high bytes is binary data.
         (qss-odbc-binary v))
        (t v)))

(defun qss-odbc--params (params)
  "PARAMS (a list) as a vector for the module."
  (vconcat (mapcar #'qss-odbc--param params)))

;;; ------------------------------------------------------------------
;;; Columns and values

(defconst qss-odbc--types
  '((1 . char) (12 . varchar) (-1 . longvarchar)
    (-8 . wchar) (-9 . wvarchar) (-10 . wlongvarchar)
    (2 . numeric) (3 . decimal)
    (4 . integer) (5 . smallint) (-6 . tinyint) (-5 . bigint)
    (7 . real) (6 . float) (8 . double)
    (-7 . bit) (16 . boolean)
    (91 . date) (92 . time) (93 . timestamp) (9 . date) (10 . time) (11 . timestamp)
    (-2 . binary) (-3 . varbinary) (-4 . longvarbinary)
    (-11 . guid))
  "ODBC SQL type codes and their names.")

(defun qss-odbc--column (raw)
  "A column description from the module's raw description
\(NAME TYPE SIZE DIGITS NULLABLE TYPE-NAME)."
  (pcase-let ((`(,name ,type ,size ,digits ,nullable ,type-name) raw))
    (list :name name
          :type (if (member (downcase (or type-name "")) '("bool" "boolean"))
                    'boolean            ; psqlODBC: VARCHAR "1"/"0"
                  (or (alist-get type qss-odbc--types) type))
          :size size :scale digits :nullable nullable
          :type-name type-name)))

(defun qss-odbc-column-name (col) "Name of the column COL." (plist-get col :name))
(defun qss-odbc-column-type (col) "Type of COL, e.g. `integer'." (plist-get col :type))
(defun qss-odbc-column-scale (col) "Digits after the point of COL." (plist-get col :scale))

(defun qss-odbc--scaled (text scale)
  "The number TEXT (\"-1094.8\") as an integer scaled by 10^SCALE.
Digits beyond SCALE must be zeros; otherwise signal an error."
  (if (not (string-match "\\`\\([-+]?\\)\\([0-9]*\\)\\(?:\\.\\([0-9]*\\)\\)?\\'" text))
      (signal 'qss-odbc-error (list (format "Not an exact number: %S" text)))
    (let* ((neg (equal (match-string 1 text) "-"))
           (int (match-string 2 text))
           (frac (or (match-string 3 text) ""))
           (extra (and (> (length frac) scale) (substring frac scale))))
      (when (and extra (string-match-p "[1-9]" extra))
        (signal 'qss-odbc-error
                (list (format "%s has more than %d digits after the point"
                              text scale))))
      (setq frac (if extra (substring frac 0 scale)
                   (concat frac (make-string (- scale (length frac)) ?0))))
      (let ((n (string-to-number (concat (if (equal int "") "0" int) frac))))
        (if neg (- n) n)))))

(defun qss-odbc-decimal-string (n scale)
  "The scaled integer N with SCALE digits after the point as text.
\(qss-odbc-decimal-string 109480 2) => \"1094.80\""
  (if (<= scale 0)
      (number-to-string n)
    (let* ((digits (number-to-string (abs n)))
           (digits (if (<= (length digits) scale)
                       (concat (make-string (- (1+ scale) (length digits)) ?0)
                               digits)
                     digits))
           (cut (- (length digits) scale)))
      (concat (if (< n 0) "-" "") (substring digits 0 cut) "."
              (substring digits cut)))))

(defun qss-odbc--time-string (v)
  "The date or time vector V from the module as text."
  (pcase v
    (`[date ,y ,m ,d] (format "%04d-%02d-%02d" y m d))
    (`[time ,h ,mi ,s] (format "%02d:%02d:%02d" h mi s))
    (`[timestamp ,y ,m ,d ,h ,mi ,s ,ns]
     (concat (format "%04d-%02d-%02d %02d:%02d:%02d" y m d h mi s)
             (if (zerop ns) ""
               (replace-regexp-in-string "0+\\'" "" (format ".%09d" ns)))))))

(defun qss-odbc-parse-time (text &optional decoded)
  "TEXT (\"2026-10-30\", \"2026-10-30 09:56:00.5\") as a Lisp time in UTC.
With DECODED, return the decoded time instead."
  (unless (string-match (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)"
                                "\\(?:[ T]\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
                                "\\(?::\\([0-9][0-9]\\)\\(\\.[0-9]+\\)?\\)?\\)?\\'")
                        text)
    (signal 'qss-odbc-error (list (format "Not a date or timestamp: %S" text))))
  (let* ((num (lambda (i) (if (match-beginning i)
                              (string-to-number (match-string i text)) 0)))
         (frac (match-string 7 text))
         (sec (if frac
                  (time-add (funcall num 6)
                            (cons (string-to-number (substring frac 1))
                                  (expt 10 (1- (length frac)))))
                (funcall num 6)))
         (d (make-decoded-time :year (funcall num 1) :month (funcall num 2)
                               :day (funcall num 3) :hour (funcall num 4)
                               :minute (funcall num 5) :second sec :zone 0
                               :dst nil)))
    (if decoded d (encode-time d))))

(defun qss-odbc--value (raw col)
  "The raw value RAW of the column COL as a Lisp value."
  (cond ((null raw) nil)
        ((vectorp raw) (qss-odbc--time-string raw))
        (t (pcase (plist-get col :type)
             ((or 'numeric 'decimal)
              (if (stringp raw) (qss-odbc--scaled raw (plist-get col :scale)) raw))
             ((or 'bit 'boolean)
              (cond ((integerp raw) (/= raw 0))
                    ((stringp raw) (and (member (downcase raw) '("1" "t" "true" "yes")) t))
                    (t raw)))
             (_ raw)))))

;;; ------------------------------------------------------------------
;;; Queries

(defun qss-odbc--handle (con)
  "The module handle of CON, or an error if CON is not a connection."
  (unless (qss-odbc-connection-p con)
    (signal 'wrong-type-argument (list 'qss-odbc-connection-p con)))
  (qss-odbc-connection-handle con))

(defun qss-odbc--rows (stmt)
  "Columns and rows of STMT: (COLUMNS . ROWS), rows as lists."
  (let* ((cols (mapcar #'qss-odbc--column (qss-odbc--columns stmt)))
         (rows nil) raw)
    (while (setq raw (qss-odbc--fetch stmt))
      (push (cl-mapcar #'qss-odbc--value raw cols) rows))
    (cons cols (nreverse rows))))

(defun qss-odbc-select (con sql &rest params)
  "Run the query SQL on CON with PARAMS; return (COLUMNS . ROWS).
COLUMNS are property lists (:name :type :size :scale :nullable
:type-name), each
row is a list of values in the order of COLUMNS."
  (let ((stmt (qss-odbc--exec (qss-odbc--handle con) sql (qss-odbc--params params))))
    (unwind-protect (qss-odbc--rows stmt)
      (qss-odbc--close stmt))))

(defun qss-odbc-query (con sql &rest params)
  "Run the query SQL on CON with PARAMS; a list of rows.
The first element is the list of column names, as `sqlite-select'
does with \\='full."
  (let ((result (apply #'qss-odbc-select con sql params)))
    (cons (mapcar #'qss-odbc-column-name (car result)) (cdr result))))

(defun qss-odbc-execute (con sql &rest params)
  "Run the statement SQL (INSERT, UPDATE, DDL ...) on CON with PARAMS.
Return the number of rows affected (-1 if the driver does not know)."
  (let ((stmt (qss-odbc--exec (qss-odbc--handle con) sql (qss-odbc--params params))))
    (unwind-protect (qss-odbc--row-count stmt)
      (qss-odbc--close stmt))))

(defun qss-odbc-select-all (con sql &rest params)
  "Like `qss-odbc-select', but a list of all result sets."
  (let ((stmt (qss-odbc--exec (qss-odbc--handle con) sql (qss-odbc--params params)))
        (sets nil))
    (unwind-protect
        (progn
          (push (qss-odbc--rows stmt) sets)
          (while (qss-odbc--more-results stmt)
            (push (qss-odbc--rows stmt) sets))
          (nreverse sets))
      (qss-odbc--close stmt))))

(defun qss-odbc-map-query (fn con sql &rest params)
  "Call FN with the values of each row of SQL on CON, one row at a time.
Only the current row is in memory.  Return nil."
  (let ((stmt (qss-odbc--exec (qss-odbc--handle con) sql (qss-odbc--params params))))
    (unwind-protect
        (let ((cols (mapcar #'qss-odbc--column (qss-odbc--columns stmt))) raw)
          (while (setq raw (qss-odbc--fetch stmt))
            (apply fn (cl-mapcar #'qss-odbc--value raw cols))))
      (qss-odbc--close stmt))))

(defmacro qss-odbc-do-query (spec &rest body)
  "Run BODY for each row of a query, the columns bound to VARS.
SPEC is (VARS CON SQL PARAMS...).  `cl-return' leaves the loop; the
statement is freed in any case.

  (qss-odbc-do-query ((id name) con \"SELECT id, name FROM t WHERE x = ?\" 5)
    (message \"%s %s\" id name))"
  (declare (indent 1) (debug ((sexp form form &rest form) body)))
  (pcase-let ((`(,vars ,con ,sql . ,params) spec))
    `(cl-block nil
       (qss-odbc-map-query (lambda (&optional ,@vars &rest _) ,@body)
                           ,con ,sql ,@params))))

;;; ------------------------------------------------------------------
;;; Catalog

(defun qss-odbc--catalog-rows (con kind args)
  "Rows of the catalog function KIND with ARGS, as property lists."
  (let ((stmt (qss-odbc--catalog (qss-odbc--handle con) kind (vconcat args))))
    (unwind-protect
        (let* ((result (qss-odbc--rows stmt))
               (keys (mapcar (lambda (c)
                               (intern (concat ":" (downcase (qss-odbc-column-name c)))))
                             (car result))))
          (mapcar (lambda (row) (cl-mapcan #'list keys row)) (cdr result)))
      (qss-odbc--close stmt))))

(defun qss-odbc-tables (con &optional type)
  "Tables of CON as property lists (:table_name :table_type ...).
TYPE, e.g. \"TABLE\" or \"VIEW\" or \"TABLE,VIEW\", narrows them."
  (qss-odbc--catalog-rows con 'tables (list nil nil nil type)))

(defun qss-odbc-table-columns (con table)
  "Columns of TABLE in CON as property lists (:column_name :type_name ...)."
  (qss-odbc--catalog-rows con 'columns (list nil nil table nil)))

(defun qss-odbc-primary-key (con table)
  "Names of the primary key columns of TABLE, in key order.
Nil if the driver does not support SQLPrimaryKeys."
  (condition-case nil
      (mapcar (lambda (r) (plist-get r :column_name))
              (sort (qss-odbc--catalog-rows con 'primary-keys (list nil nil table))
                    (lambda (a b) (< (plist-get a :key_seq) (plist-get b :key_seq)))))
    (qss-odbc-error nil)))

(provide 'qss-odbc)
;;; qss-odbc.el ends here
