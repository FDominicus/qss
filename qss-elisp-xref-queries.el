;;; qss-elisp-xref-queries.el --- Questions to the Elisp cross references  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Examples for reading the database of `qss-elisp-xref' with qss.
;; Every form is one question: put point after it and type C-x C-e; it
;; opens as a table.  The symbol asked about is a string, since the
;; database holds names, not symbols.
;;
;; (xref "TABLE") is a table of the database as a source;
;; `qss-elisp-xref-database' says which file (bind it to ask another).
;; (qss-display-> ...) is (qss-display (qss-> ...)).  Columns are written as
;; keywords.  The answers that select :path and :line open the file
;; there with RET.

;;; Code:

(require 'qss)

;;; Where is it defined, and what does it say about itself?

(qss-display-> (xref "definitions") (where (= :name "seq-reduce"))
               (join (xref "files") :on (:file-id :id))
               (select :library :kind :arglist :line :doc :path))

;;; Where is it used?  With the context: called, quoted, #', ...

(qss-display-> (xref "occurrences") (where (= :value "string-trim"))
               (join (xref "files") :on (:file-id :id))
               (select :library :line :col :context :parent-head :path)
               (sort-by :library))

;;; Who calls it (after macro expansion, from the compiler)?

;; The line is that of the caller's definition; a caller at the top
;; level (nil) has none.
(qss-display-> (xref "calls") (where (= :callee "string-trim"))
               (join (xref "definitions") :on ((:file-id :file-id) (:caller :name)) :kind left)
               (join (xref "files") :on (:file-id :id))
               (select :library :caller :line :path)
               (sort-by :caller))

;;; What does a function call?

(qss-display-> (xref "calls") (where (= :caller "vtable-insert"))
               (select :callee) (sort-by :callee))

;;; The methods of a generic function, with what they specialize on

(qss-display-> (xref "definitions") (where (and (= :name "cl-print-object") (= :kind "method")))
               (join (xref "files") :on (:file-id :id))
               (select :library :specializers :line :path))

;;; The texts a package shows to people: start of a translation

(qss-display-> (xref "occurrences")
               (where (and (= :context "string")
                      (in :parent-head '("message" "user-error" "error" "format-message"
                                         "read-string" "y-or-n-p" "yes-or-no-p"))))
               (join (xref "files") :on (:file-id :id))
               (where (= :library "vtable"))
               (select :parent-head :line :col :value :path))

;;; Which functions show the most texts, over everything

(qss-display-> (xref "occurrences") (where (= :context "string"))
               (group-by :parent-head) (sort-by :count :desc) (first 20))

;;; The macros used most

(qss-display-> (xref "occurrences") (where (= :context "macro-use"))
               (group-by :value) (sort-by :count :desc) (first 20))

;;; Who still uses what became obsolete (here: defadvice, when-let)

(qss-display-> (xref "occurrences") (where (in :value '("defadvice" "when-let" "if-let")))
               (join (xref "files") :on (:file-id :id))
               (group-by :origin :package :value) (sort-by :count :desc))

;;; Local functions (cl-flet, cl-labels) and where they live

(qss-display-> (xref "definitions") (where (in :kind '("local-function" "local-macro")))
               (join (xref "files") :on (:file-id :id))
               (select :library :name :kind :arglist :line :path) (first 50))

;;; The source text of a definition

(qss-display-> (xref "definitions") (where (= :name "seq-reduce"))
               (join (xref "forms") :on (:form-id :id))
               (select :name :start-line :end-line :text))

;;; What there is: kinds of definitions, files by origin (emacs, elpa, user)

(qss-display-> (xref "definitions") (group-by :kind) (sort-by :count :desc))
(qss-display-> (xref "files") (group-by :origin :package) (sort-by :count :desc))

;;; Files that could not be read or compiled

(qss-display-> (xref "files") (where (not-null :error)) (select :path :error))
(qss-display-> (xref "compiled") (where (not-null :error))
               (join (xref "files") :on (:file-id :id)) (select :path :error))

(provide 'qss-elisp-xref-queries)
;;; qss-elisp-xref-queries.el ends here
