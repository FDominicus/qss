;;; qss-examples.el --- Examples for qss  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; Examples of qss, to read and to run.  Not loaded by (require 'qss).
;;
;;   M-x qss-examples      show all examples with their results in a
;;                         buffer; [show] displays a result as a table
;;
;; Or go through `qss-examples' form by form: put point after a form in
;; this file and press C-x C-e, or copy it into IELM.
;;
;; The SQLite examples use the small demo database that
;; `qss-demo-database' creates in `temporary-file-directory'.  The
;; results in the comments are those of that database; examples on files
;; and processes depend on your machine.

;;; Code:

(require 'qss)

;;; ------------------------------------------------------------------
;;; Own row type: buffers

;; A row type is a few methods.  Here: buffers as rows, so that
;; (qss-examples-buffers) is a source like files and processes.

(defun qss-examples-buffers ()
  "Return a table of the live buffers."
  (qss-make-table "buffers" (buffer-list) nil #'qss-examples-buffers))

(cl-defmethod qss-columns ((_row buffer))
  "Return the columns of a buffer row."
  '((name :width 30) mode (size :type number) file))

(cl-defmethod qss-field ((row buffer) name)
  "Return field NAME of the buffer ROW."
  (pcase name
    ('name (buffer-name row))
    ('mode (symbol-name (buffer-local-value 'major-mode row)))
    ('size (buffer-size row))
    ('file (buffer-file-name row))
    ('modified (buffer-modified-p row))))

(cl-defmethod qss-default-action ((row buffer))
  "Switch to the buffer ROW."
  (pop-to-buffer row))

(cl-defmethod qss-actions ((row buffer))
  "Return the actions for the buffer ROW."
  (ignore row)
  (list (cons "Switch to" #'pop-to-buffer)
        (cons "Kill" (lambda (b) (when (yes-or-no-p (format "Kill %s? " (buffer-name b)))
                                   (kill-buffer b))))))

;;; ------------------------------------------------------------------
;;; The examples

(defvar qss-examples
  '(("Files: the largest Emacs Lisp files in your Emacs directory"
     (qss-> (files user-emacs-directory)
            (where (like :name "%.el"))
            (sort-by size :desc)
            (first 5)
            (select name size modified)))

    ("Files: sizes written as in Nushell"
     (qss-> (files user-emacs-directory) (where (> :size "4kb")) (select name size)))

    ("Files: changed in the last seven days; RET still opens the file"
     (let ((since (time-subtract nil (days-to-time 7))))
       (qss-> (files user-emacs-directory)
              (where (and (= :type "file") (> :modified since)))
              (sort-by modified :desc))))

    ("Processes: the five largest by memory"
     (qss-> (processes) (sort-by rss :desc) (first 5) (select pid user rss command)))

    ("Processes: how many per user"
     (qss-> (processes) (group-by user) (sort-by count :desc)))

    ("Own row type: buffers visiting a file, largest first"
     (qss-> (qss-examples-buffers) (where (not-null :file)) (sort-by size :desc)))

    ("Any list of property lists is a table"
     ;; => ((:name "Ada" :born 1815) (:name "Grace" :born 1906))
     (qss-> '((:name "Ada" :born 1815) (:name "Alan" :born 1912) (:name "Grace" :born 1906))
            (where (< :born 1910))
            (sort-by born)
            collect))

    ("NULL as in SQL: (not (= ...)) does not keep rows without a value"
     ;; => ("Bob Stone")   Dan has no city: unknown, so not kept
     (qss-> (sqlite (qss-demo-database) "Customers")
            (where (not (= :city "Bruchsal")))
            (get name)
            collect))

    ("SQLite: the tables of a database; RET on one shows its rows"
     (qss-> (tables (qss-demo-database))))

    ("SQLite: the columns of a table, with their names in the database"
     ;; => order-no OrderNo INTEGER, customer-no CustomerNo INTEGER, ...
     (qss-> (sqlite (qss-demo-database) "Orders") columns))

    ("SQLite: a pipe becomes one query"
     ;; => (4715 4713 4716 4711)
     (qss-> (sqlite (qss-demo-database) "Orders")
            (where (> :amount 100))
            (sort-by amount :desc)
            (select order-no amount)))

    ("SQLite: the SQL of that pipe"
     ;; SELECT "OrderNo", "Amount" FROM "Orders" WHERE ("Amount" > ?)
     ;; ORDER BY "Amount" IS NULL, "Amount" DESC   Parameters: (100)
     (qss-> (sqlite (qss-demo-database) "Orders")
            (where (> :amount 100))
            (sort-by amount :desc)
            (select order-no amount)
            sql))

    ("SQLite: Lisp values become parameters, lists work with in"
     ;; => (4711 4713 4714 4716)
     (let ((customers '(1 3)) (from "2026-09-01"))
       (qss-> (sqlite (qss-demo-database) "Orders")
              (where (and (in :customer-no customers) (>= :order-date from)))
              (get order-no)
              collect)))

    ("SQLite: open orders, i.e. Status is NULL"
     ;; => 2
     (qss-> (sqlite (qss-demo-database) "Orders") (where (is-null :status)) count))

    ("SQLite: like ignores case, as in SQL"
     ;; => ("Anna Meier" "Clara Weiss")
     (qss-> (sqlite (qss-demo-database) "Customers")
            (where (or (like :name "%MEIER") (like :name "c%")))
            (get name)
            collect))

    ("SQLite: first SQL, then Lisp with it (fetched rows, in memory)"
     ;; => ("Bob Stone" "Dan Brown")
     (qss-> (sqlite (qss-demo-database) "Customers")
            (where (> :since "2020-01-01"))
            (where (string-match-p "o" (plist-get it :name)))
            (get name)
            collect))

    ("SQLite: orders per status (group-by runs in memory)"
     ;; => paid 3, nil 2, cancelled 1
     (qss-> (sqlite (qss-demo-database) "Orders") (group-by status) (sort-by count :desc)))

    ("Using the rows in Lisp: the total of the paid orders"
     ;; => 245.5
     (let ((amounts (qss-> (sqlite (qss-demo-database) "Orders")
                           (where (= :status "paid"))
                           (get amount)
                           collect)))
       (apply #'+ amounts)))

    ("Joining by hand: orders with the customer's name"
     ;; => ((:order-no 4711 :name "Anna Meier" :amount 120.0) ...)
     (let* ((customers (qss-> (sqlite (qss-demo-database) "Customers") collect))
            (names (mapcar (lambda (c) (cons (plist-get c :customer-no) (plist-get c :name)))
                           customers)))
       (qss-> (sqlite (qss-demo-database) "Orders")
              (where (not-null :status))
              (each (list :order-no (plist-get it :order-no)
                          :name (cdr (assq (plist-get it :customer-no) names))
                          :amount (plist-get it :amount))))))

    ("Text: a table as Org, to paste into a note (w in a table does the same)"
     ;; => | name        | city     | ...
     (qss-> (sqlite (qss-demo-database) "Customers") (select name city since) (to org)))

    ("Text: CSV with the plain values, for other programs"
     ;; sizes in bytes, times with seconds
     (qss-> (files user-emacs-directory) (where (= :type "file")) (select name size modified)
            (first 3) (to csv)))

    ("Nushell: any command becomes a table (needs nu)"
     (qss-> (nu "ls") (where (= :type "file")) (sort-by size :desc) (first 5))
     (executable-find qss-nu-program))

    ("Nushell: a record is shown as column and value (needs nu)"
     (qss-> (nu "sys host"))
     (executable-find qss-nu-program))

    ("Nushell: data from a file Nushell can open, then qss words (needs nu)"
     ;; nu reads CSV, JSON, TOML, YAML, Excel, SQLite ...
     (let ((csv (expand-file-name "qss-example.csv" temporary-file-directory)))
       (with-temp-file csv (insert "item,amount\nnails,12.5\nscrews,99\nglue,4\n"))
       (qss-> (nu (format "open %s" csv)) (where (> :amount 10)) (sort-by amount :desc)))
     (executable-find qss-nu-program))

    ("Nushell: durations stay durations (needs nu)"
     (qss-> (nu "[{task: build, took: 3min} {task: test, took: 95sec} {task: lint, took: 800ms}]")
            (where (> :took "1sec"))
            (sort-by took :desc))
     (executable-find qss-nu-program))

    ("Nushell in the middle of a pipe: a qss table goes in, a table comes out (needs nu)"
     ;; size and modified arrive in Nushell as file size and datetime
     (qss-> (files user-emacs-directory)
            (where (= :type "file"))
            (nu "sort-by modified -r | first 5 | select name size modified"))
     (executable-find qss-nu-program))

    ("Nushell in the middle of a pipe: a single value comes back as it is (needs nu)"
     (qss-> (files user-emacs-directory) (nu "get size | math sum"))
     (executable-find qss-nu-program))

    ("Nushell in the middle of a pipe: SQLite rows saved as CSV (needs nu)"
     (let ((csv (expand-file-name "qss-open-orders.csv" temporary-file-directory)))
       (qss-> (sqlite (qss-demo-database) "Orders") (where (is-null :status))
              (nu (format "to csv | save -f %s" csv)))
       (qss-> (nu (format "open %s" csv))))
     (and (executable-find qss-nu-program) (sqlite-available-p))))

  "Examples of qss: a list of (TITLE FORM [CONDITION]).
FORM is evaluated with lexical binding; see `qss-examples'.  If
CONDITION is given and evaluates to nil, the example is skipped.")

;;; ------------------------------------------------------------------
;;; Showing the examples

(defun qss-examples--format (form &optional indent)
  "Return FORM as code text, starting at column INDENT.
A `qss->' pipe gets one stage per line, `let' and `let*' one binding
per line; everything else is printed on one line."
  (let ((indent (or indent 0)))
    (pcase form
      (`(qss-> ,source . ,stages)
       (let ((pad (make-string (+ indent 7) ?\s)))
         (concat "(qss-> " (qss-examples--format source (+ indent 7))
                 (mapconcat (lambda (stage) (concat "\n" pad (prin1-to-string stage)))
                            stages "")
                 ")")))
      (`(,(and head (or 'let 'let*)) ,bindings . ,body)
       (let* ((open (format "(%s (" head))
              (bpad (make-string (+ indent (length open)) ?\s))
              (bodypad (make-string (+ indent 2) ?\s)))
         (concat open
                 (mapconcat (lambda (b)
                              (let ((pre (format "(%s " (car b))))
                                (concat pre
                                        (qss-examples--format
                                         (cadr b) (+ indent (length open) (length pre)))
                                        ")")))
                            bindings (concat "\n" bpad))
                 ")"
                 (mapconcat (lambda (f)
                              (concat "\n" bodypad (qss-examples--format f (+ indent 2))))
                            body "")
                 ")")))
      (_ (prin1-to-string form)))))

(defun qss-examples--summary (value)
  "Return a short text for the result VALUE of an example."
  (cond ((qss-table-p value)
         (format "table: %s  (%d rows)" (qss-table-title value) (qss-count value)))
        ((qss-relation-p value)
         (format "query: %s  (%d rows)" (qss-relation-title value) (qss-count value)))
        ((stringp value) value)
        (t (let ((print-length 4) (print-level 3)) (format "%S" value)))))

;;;###autoload
(defun qss-examples ()
  "Show all `qss-examples' with their results in a buffer.
The [show] button after a table or a query displays it."
  (interactive)
  (let ((buf (get-buffer-create "*qss examples*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emacs-lisp-mode)
        (insert ";; qss examples.  [show] displays a result; the forms can be\n"
                ";; evaluated here with C-x C-e as well.\n\n")
        (dolist (example qss-examples)
          (let* ((title (car example))
                 (form (cadr example))
                 (ready (or (null (cddr example)) (eval (nth 2 example) t)))
                 (value (if (not ready)
                            "skipped: needs Nushell (see `qss-nu-program')"
                          (condition-case err (eval form t)
                            (error (list 'error (error-message-string err)))))))
            (insert ";;; " title "\n")
            (insert (qss-examples--format form) "\n")
            (insert ";; => " (replace-regexp-in-string
                              "\n" "\n;;    " (qss-examples--summary value)))
            (when (or (qss-table-p value) (qss-relation-p value))
              (insert "  ")
              (insert-text-button "[show]"
                                  'action (lambda (_) (qss-display value))
                                  'follow-link t))
            (insert "\n\n"))))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(provide 'qss-examples)
;;; qss-examples.el ends here
