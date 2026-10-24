;;; qss-nuon.el --- Read and write NUON, the data format of Nushell  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; NUON (Nushell Object Notation, `to nuon' and `from nuon') is JSON
;; with Nushell's types: sizes (10kb), durations (3hr), datetimes,
;; ranges, binary data, and a table syntax.
;;
;;   (qss-nuon-read "[[name, size]; [foo, 10kb]]")
;;   => (((name . "foo") (size . #s(qss-nuon-size 10000))))
;;
;; `qss-nuon-read' returns what `json-parse-string' returns with
;; :object-type alist, :array-type list, :null-object :null and
;; :false-object nil, plus these values:
;;
;;   10kb          #s(qss-nuon-size 10000)        bytes
;;   3hr           #s(qss-nuon-duration ...)      nanoseconds
;;   2026-10-22T…  a Lisp time
;;   1..5          the text "1..5"
;;   0x[0aff]      the unibyte string "\n\377"
;;   NaN, inf      the floats 0.0e+NaN, 1.0e+INF
;;
;; `qss-nuon-plain' turns sizes and durations into plain numbers.
;; `qss-nuon-encode' writes Lisp values as NUON for `from nuon'.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'qss-table)
(require 'qss-pipe)

(cl-defstruct (qss-nuon-size (:constructor qss-nuon-size (bytes)))
  "A size from NUON, in bytes."
  bytes)

(cl-defstruct (qss-nuon-duration (:constructor qss-nuon-duration (ns)))
  "A duration from NUON, in nanoseconds."
  ns)

(define-error 'qss-nuon-error "Cannot read NUON")

;;; ------------------------------------------------------------------
;;; Reading

(defconst qss-nuon--datetime-regexp
  (concat "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]"
          "\\(?:T[0-9][0-9]:[0-9][0-9]\\(?::[0-9][0-9]\\(?:\\.[0-9]+\\)?\\)?"
          "\\(?:Z\\|[+-][0-9][0-9]:?[0-9][0-9]\\)?\\)?\\'")
  "A datetime as Nushell writes it; the time and the zone may be missing.")

(defconst qss-nuon--number-regexp
  "\\`-?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "A number.")

(defun qss-nuon--fail (what)
  "Signal that WHAT was expected at point."
  (signal 'qss-nuon-error
          (list (format "%s expected at position %d: %S" what (point)
                        (buffer-substring (point) (min (point-max) (+ (point) 20)))))))

(defun qss-nuon--skip ()
  "Skip whitespace and comments (# to the end of the line)."
  (while (progn (skip-chars-forward " \t\n\r")
                (when (eq (char-after) ?#)
                  (forward-line 1)
                  t))))

(defun qss-nuon--skip-separator ()
  "Skip whitespace, comments and a comma."
  (qss-nuon--skip)
  (when (eq (char-after) ?,)
    (forward-char)
    (qss-nuon--skip)))

(defun qss-nuon--expect (char)
  "Move over CHAR, or fail."
  (qss-nuon--skip)
  (if (eq (char-after) char)
      (forward-char)
    (qss-nuon--fail (format "`%c'" char))))

(defun qss-nuon--read-escape ()
  "Read the escape after a backslash; return its text."
  (let ((c (char-after)))
    (forward-char)
    (pcase c
      (?n "\n") (?t "\t") (?r "\r") (?b "\b") (?f "\f") (?e "\e") (?0 "\0")
      (?u (cond ((looking-at "{\\([0-9a-fA-F]+\\)}")
                 (goto-char (match-end 0))
                 (string (string-to-number (match-string 1) 16)))
                ((looking-at "[0-9a-fA-F]\\{4\\}")
                 (goto-char (match-end 0))
                 (string (string-to-number (match-string 0) 16)))
                (t (qss-nuon--fail "Unicode escape"))))
      ('nil (qss-nuon--fail "escape"))
      (_ (string c)))))

(defun qss-nuon--read-double-quoted ()
  "Read a string in double quotes, point after the opening quote."
  (let ((parts nil))
    (while (progn (let ((start (point)))
                    (skip-chars-forward "^\"\\\\")
                    (push (buffer-substring-no-properties start (point)) parts))
                  (pcase (char-after)
                    (?\\ (forward-char) (push (qss-nuon--read-escape) parts) t)
                    (?\" (forward-char) nil)
                    (_ (qss-nuon--fail "closing \"")))))
    (apply #'concat (nreverse parts))))

(defun qss-nuon--read-literal (close)
  "Read up to the character CLOSE, without escapes; point after the opener."
  (let ((start (point)))
    (unless (search-forward (string close) nil t)
      (qss-nuon--fail (format "closing %c" close)))
    (buffer-substring-no-properties start (1- (point)))))

(defun qss-nuon--read-raw ()
  "Read a raw string r#'...'#, point on the r."
  (unless (looking-at "r\\(#+\\)'")
    (qss-nuon--fail "raw string"))
  (let ((end (concat "'" (match-string 1)))
        (start (match-end 0)))
    (goto-char start)
    (unless (search-forward end nil t)
      (qss-nuon--fail "end of raw string"))
    (buffer-substring-no-properties start (- (point) (length end)))))

(defun qss-nuon--read-binary ()
  "Read binary data 0x[...], point on the 0."
  (forward-char 3)
  (let ((start (point)))
    (unless (search-forward "]" nil t) (qss-nuon--fail "`]'"))
    (let ((hex (replace-regexp-in-string "[^0-9a-fA-F]" ""
                                         (buffer-substring-no-properties start (1- (point))))))
      (apply #'unibyte-string
             (cl-loop for i from 0 below (length hex) by 2
                      collect (string-to-number (substring hex i (+ i 2)) 16))))))

(defconst qss-nuon--datetime-parts-regexp
  (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)"
          "\\(?:T\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\(?::\\([0-9][0-9]\\)\\(?:\\.\\([0-9]+\\)\\)?\\)?"
          "\\(Z\\|\\([+-]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)\\)?\\)?\\'")
  "The parts of a datetime, for `qss-nuon--time'.")

(defun qss-nuon--days-from-civil (y m d)
  "Return the number of days from 1970-01-01 to year Y, month M, day D.
The calendar is the proleptic Gregorian one."
  ;; Howard Hinnant, "chrono-Compatible Low-Level Date Algorithms"
  (let* ((y (if (<= m 2) (1- y) y))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (/ (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (1- d)))
         (doe (+ (* yoe 365) (/ yoe 4) (- (/ yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun qss-nuon--time (text)
  "Return the datetime TEXT as a Lisp time; without a zone it is UTC.
Computed directly: `iso8601-parse' and `encode-time' are slow for
thousands of rows, and this keeps the nanoseconds."
  (unless (string-match qss-nuon--datetime-parts-regexp text)
    (signal 'qss-nuon-error (list (format "Not a datetime: %S" text))))
  (cl-flet ((n (i) (let ((m (match-string i text))) (if m (string-to-number m) 0))))
    (let* ((zone (if (match-beginning 9)
                     (* (if (equal (match-string 9 text) "-") -1 1)
                        (+ (* 3600 (n 10)) (* 60 (n 11))))
                   0))
           (fraction (match-string 7 text))
           (secs (- (+ (* 86400 (qss-nuon--days-from-civil (n 1) (n 2) (n 3)))
                       (* 3600 (n 4)) (* 60 (n 5)) (n 6))
                    zone)))
      (if fraction
          ;; in the form of `qss-time': .000000000 is a whole second
          (let ((hz (expt 10 (length fraction))))
            (qss--canonical-time (cons (+ (* secs hz) (string-to-number fraction)) hz)))
        (list (ash secs -16) (logand secs 65535))))))

(defun qss-nuon--token-value (text)
  "Return the value of the bare TEXT: number, size, duration, date or string."
  (let ((c (aref text 0)))
    (if (or (and (>= c ?0) (<= c ?9)) (memq c '(?- ?.)))
        ;; numbers, sizes, durations, dates and ranges start like this
        (cond ((string-match-p qss-nuon--number-regexp text) (string-to-number text))
              ((equal text "-inf") -1.0e+INF)
              ((let ((q (qss-parse-quantity text)))
                 (and q (if (eq (car q) 'size)
                            (qss-nuon-size (cdr q))
                          (qss-nuon-duration (cdr q))))))
              ((string-match-p qss-nuon--datetime-regexp text)
               (condition-case nil (qss-nuon--time text) (error text)))
              ;; ranges (1..5) stay text
              (t text))
      (pcase text
        ("true" t)
        ("false" nil)
        ("null" :null)
        ("NaN" 0.0e+NaN)
        ("inf" 1.0e+INF)
        (_ text)))))

(defun qss-nuon--read-bare (key)
  "Read a bare word.  As a KEY it ends before a colon as well."
  (let ((start (point)))
    (skip-chars-forward (if key "^ \t\n\r,:;[]{}\"'" "^ \t\n\r,;[]{}\"'"))
    (when (= start (point)) (qss-nuon--fail "value"))
    (buffer-substring-no-properties start (point))))

(defun qss-nuon--read-key ()
  "Read the key of a record field and return it as a symbol."
  (qss-nuon--skip)
  (intern
   (pcase (char-after)
     (?\" (forward-char) (qss-nuon--read-double-quoted))
     (?' (forward-char) (qss-nuon--read-literal ?'))
     (?` (forward-char) (qss-nuon--read-literal ?`))
     (_ (qss-nuon--read-bare t)))))

(defun qss-nuon--read-record ()
  "Read a record, point after the brace; return an alist."
  (let ((fields nil))
    (qss-nuon--skip)
    (while (not (eq (char-after) ?\}))
      (when (eobp) (qss-nuon--fail "`}'"))
      (let ((key (qss-nuon--read-key)))
        (qss-nuon--expect ?:)
        (push (cons key (qss-nuon--read-value)) fields))
      (qss-nuon--skip-separator))
    (forward-char)
    (nreverse fields)))

(defun qss-nuon--read-elements ()
  "Read values up to the closing bracket; return them as a list."
  (let ((elements nil))
    (qss-nuon--skip)
    (while (not (memq (char-after) '(?\] ?\;)))
      (when (eobp) (qss-nuon--fail "`]'"))
      (push (qss-nuon--read-value) elements)
      (qss-nuon--skip-separator))
    (nreverse elements)))

(defun qss-nuon--read-list ()
  "Read a list or a table, point after the bracket.
A table [[a, b]; [1, 2], [3, 4]] becomes a list of records."
  (let ((elements (qss-nuon--read-elements)))
    (if (not (eq (char-after) ?\;))
        (progn (forward-char) elements)
      ;; a table: ELEMENTS holds the column names
      (forward-char)
      (let ((columns (mapcar (lambda (c) (intern (format "%s" c)))
                             (car elements)))
            (rows (qss-nuon--read-elements)))
        (unless (eq (char-after) ?\]) (qss-nuon--fail "`]'"))
        (forward-char)
        (mapcar (lambda (row) (cl-mapcar #'cons columns row)) rows)))))

(defun qss-nuon--read-value ()
  "Read the value at point."
  (qss-nuon--skip)
  (pcase (char-after)
    ('nil (qss-nuon--fail "value"))
    (?\{ (forward-char) (qss-nuon--read-record))
    (?\[ (forward-char) (qss-nuon--read-list))
    (?\" (forward-char) (qss-nuon--read-double-quoted))
    (?' (forward-char) (qss-nuon--read-literal ?'))
    (?` (forward-char) (qss-nuon--read-literal ?`))
    (_ (cond ((looking-at-p "r#+'") (qss-nuon--read-raw))
             ((looking-at-p "0x\\[") (qss-nuon--read-binary))
             (t (qss-nuon--token-value (qss-nuon--read-bare nil)))))))

(defun qss-nuon-read (text)
  "Return the value of the NUON TEXT.
Empty TEXT is :null.  See the commentary of qss-nuon.el for the
values; errors signal `qss-nuon-error'."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (qss-nuon--skip)
    (if (eobp)
        :null
      (prog1 (qss-nuon--read-value)
        (qss-nuon--skip)
        (unless (eobp) (qss-nuon--fail "end of text"))))))

(defun qss-nuon-plain (value)
  "Return VALUE with sizes and durations as plain numbers, everywhere in it."
  (cond ((qss-nuon-size-p value) (qss-nuon-size-bytes value))
        ((qss-nuon-duration-p value) (qss-nuon-duration-ns value))
        ((consp value)
         (let ((out nil))
           (while (consp value)
             (push (qss-nuon-plain (pop value)) out))
           ;; VALUE is now the tail: nil, or the end of a dotted list
           (nconc (nreverse out) (qss-nuon-plain value))))
        (t value)))

;;; ------------------------------------------------------------------
;;; Writing

(defun qss-nuon--string (s)
  "Return the string S in double quotes, with escapes."
  (concat "\""
          (replace-regexp-in-string
           "[\"\\\\[:cntrl:]]"
           (lambda (m)
             (pcase (aref m 0)
               (?\" "\\\\\"") (?\\ "\\\\\\\\") (?\n "\\\\n") (?\t "\\\\t") (?\r "\\\\r")
               (c (format "\\\\u{%x}" c))))
           s t)
          "\""))

(defun qss-nuon--float (f)
  "Return the float F as NUON."
  (cond ((isnan f) "NaN")
        ((= f 1.0e+INF) "inf")
        ((= f -1.0e+INF) "-inf")
        ;; Nushell reads 3.0 and 1e+20 as Emacs writes them
        (t (number-to-string f))))

(defun qss-nuon-encode (value &optional type)
  "Return VALUE as NUON text.
TYPE says what a number or time is: `size' (bytes), `duration'
\(nanoseconds) or `time'.  Without TYPE, numbers stay numbers and a
list of integers stays a list; only the column type of a table can
tell a time from a list of two numbers.  Property lists become
records, other lists lists, nil is null and t is true."
  (cond ((null value) "null")
        ((eq value t) "true")
        ((eq value :null) "null")
        ((or (and (eq type 'time) (qss--time-p value))
             ;; (TICKS . HZ) cannot be a list
             (and (consp value) (integerp (car value)) (integerp (cdr value))))
         (format-time-string "%FT%T.%N%:z" value))
        ((qss-nuon-size-p value) (format "%db" (qss-nuon-size-bytes value)))
        ((qss-nuon-duration-p value) (format "%dns" (qss-nuon-duration-ns value)))
        ((and (eq type 'size) (numberp value)) (format "%db" (round value)))
        ((and (eq type 'duration) (numberp value)) (format "%dns" (round value)))
        ((integerp value) (number-to-string value))
        ((floatp value) (qss-nuon--float value))
        ((stringp value) (qss-nuon--string value))
        ((and (consp value) (keywordp (car value)))
         (concat "{"
                 (mapconcat (lambda (cell)
                              (concat (qss-nuon--string (substring (symbol-name (car cell)) 1))
                                      ": " (qss-nuon-encode (cdr cell))))
                            (cl-loop for (k v) on value by #'cddr collect (cons k v))
                            ", ")
                 "}"))
        ((consp value)
         (concat "[" (mapconcat #'qss-nuon-encode value ", ") "]"))
        ((symbolp value) (qss-nuon--string (symbol-name value)))
        (t (qss-nuon--string (format "%s" value)))))

(defun qss-nuon-encode-table (table)
  "Return the rows of TABLE as NUON: a list of records with its columns.
Time, size and duration columns are written with their type.  Rows
that are single values (as after `get') become plain values with the
type of the only column."
  (let* ((specs (qss-table-columns table))
         (type-of (lambda (c) (plist-get (cdr (assq c specs)) :type)))
         (value-type (and (= (length specs) 1) (funcall type-of (caar specs)))))
    (concat
     "["
     (mapconcat
      (lambda (row)
        (if (qss-nuon--plain-row-p row)
            (qss-nuon-encode (qss-field row 'value) value-type)
          (concat "{"
                  (mapconcat (lambda (spec)
                               (let ((c (car spec)))
                                 (concat (qss-nuon--string (symbol-name c)) ": "
                                         (qss-nuon-encode (qss-field row c)
                                                          (funcall type-of c)))))
                             specs ", ")
                  "}")))
      (qss-table-rows table) ", ")
     "]")))

(defun qss-nuon--plain-row-p (row)
  "Return non-nil if ROW is a single value (as after `get'), not a record."
  (or (qss-value-p row)
      (not (or (consp row) (recordp row)))
      (and (consp row) (not (keywordp (car row))))))

(provide 'qss-nuon)
;;; qss-nuon.el ends here
