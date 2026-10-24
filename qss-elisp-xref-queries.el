;;; qss-elisp-xref-queries.el --- Questions to the Elisp cross references  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Examples for reading the database of `qss-elisp-xref' with qss.
;; Every form is one question: put point after it and type C-x C-e;
;; wrapped in `qss-display' it opens as a table.  The symbol asked about
;; is a string, since the database holds names, not symbols.
;;
;; Evaluate the three forms below first.  The answers that select path
;; and line open the file there with RET.

;;; Code:

(require 'qss)

(defvar qss-xq-db (expand-file-name "elisp-xref.sqlite3" user-emacs-directory)
  "The database the questions ask.")

(defun qss-xq (table)
  "The TABLE of the cross reference database, as a qss source."
  (qss-sqlite qss-xq-db table))

;;; Where is it defined, and what does it say about itself?

(qss-display
 (qss-> (qss-xq "definitions") (where (= :name "seq-reduce"))
        (join (qss-xq "files") :on (file-id id))
        (select library kind arglist line doc path)))

;;; Where is it used?  With the context: called, quoted, #', ...

(qss-display
 (qss-> (qss-xq "occurrences") (where (= :value "string-trim"))
        (join (qss-xq "files") :on (file-id id))
        (select library line col context parent-head path)
        (sort-by library)))

;;; Who calls it (after macro expansion, from the compiler)?

;; The line is that of the caller's definition; a caller at the top
;; level (nil) has none.
(qss-display
 (qss-> (qss-xq "calls") (where (= :callee "string-trim"))
        (join (qss-xq "definitions") :on ((file-id file-id) (caller name)) :kind left)
        (join (qss-xq "files") :on (file-id id))
        (select library caller line path)
        (sort-by caller)))

;;; What does a function call?

(qss-display
 (qss-> (qss-xq "calls") (where (= :caller "vtable-insert"))
        (select callee) (sort-by callee)))

;;; The methods of a generic function, with what they specialize on

(qss-display
 (qss-> (qss-xq "definitions") (where (and (= :name "cl-print-object") (= :kind "method")))
        (join (qss-xq "files") :on (file-id id))
        (select library specializers line path)))

;;; The texts a package shows to people: start of a translation

(qss-display
 (qss-> (qss-xq "occurrences")
        (where (and (= :context "string")
                    (in :parent-head '("message" "user-error" "error" "format-message"
                                       "read-string" "y-or-n-p" "yes-or-no-p"))))
        (join (qss-xq "files") :on (file-id id))
        (where (= :library "vtable"))
        (select parent-head line col value path)))

;;; Which functions show the most texts, over everything

(qss-display
 (qss-> (qss-xq "occurrences") (where (= :context "string"))
        (group-by parent-head) (sort-by count :desc) (first 20)))

;;; The macros used most

(qss-display
 (qss-> (qss-xq "occurrences") (where (= :context "macro-use"))
        (group-by value) (sort-by count :desc) (first 20)))

;;; Local functions (cl-flet, cl-labels) and where they live

(qss-display
 (qss-> (qss-xq "definitions") (where (in :kind '("local-function" "local-macro")))
        (join (qss-xq "files") :on (file-id id))
        (select library name kind arglist line path) (first 50)))

;;; The source text of a definition

(qss-display
 (qss-> (qss-xq "definitions") (where (= :name "seq-reduce"))
        (join (qss-xq "forms") :on (form-id id))
        (select name start-line end-line text)))

;;; What there is: kinds of definitions, files by origin (emacs, elpa, user)

(qss-display (qss-> (qss-xq "definitions") (group-by kind) (sort-by count :desc)))
(qss-display (qss-> (qss-xq "files") (group-by origin package) (sort-by count :desc)))

;;; Files that could not be read or compiled

(qss-display (qss-> (qss-xq "files") (where (not-null :error)) (select path error)))
(qss-display (qss-> (qss-xq "compiled") (where (not-null :error))
                 (join (qss-xq "files") :on (file-id id)) (select path error)))

(provide 'qss-elisp-xref-queries)
;;; qss-elisp-xref-queries.el ends here
