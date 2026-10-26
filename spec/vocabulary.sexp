;;;; vocabulary.sexp -- the words of qss, one list for Common Lisp, Emacs Lisp and Nushell
;;;;
;;;; This file decides what a word means; cases.sexp shows it with data, and
;;;; each implementation is tested against both. The same file lives in
;;;; qss-query/spec/ (Common Lisp) and qss/spec/ (Emacs Lisp); change both.
;;;;
;;;; :word     the word in a pipe: (qss-> ...) in Emacs Lisp, (query ...) in CL
;;;; :args     the arguments, as written
;;;; :nushell  the Nushell command (NIL: none)
;;;; :elisp    the function in Emacs Lisp
;;;; :cl       the function in Common Lisp; another name where CL has the
;;;;           word already (CL:FIRST ...). In QUERY the word is matched by
;;;;           name, so CL:FIRST is never called.
;;;; :ends     the word gives a value, not a table: it ends the pipe
;;;; :status   :both (both have it), :elisp or :cl (only one so far), :planned
;;;;
;;;; Rules for all words
;;;;   - Columns are keywords: :city. After a join CL also has (:table :column).
;;;;   - Conditions are data: (= :city "Bruchsal"), (and ...), (or ...), (not ...),
;;;;     = /= < > <= >= like in is-null not-null. Anything else is a value.
;;;;   - NULL (nil) as in Nushell: = and /= compare it like a value (NULL = NULL,
;;;;     NULL /= "x"); (= :city nil) finds the empty ones; in finds NULL only
;;;;     when the list has nil. < > <= >= with NULL are not true: WHERE drops
;;;;     the row (Nushell too); inside not/and/or Nushell stops with an error,
;;;;     qss goes on with SQL's "unknown".
;;;;   - Numbers compare by value: 10 = 10.0. Text by code points; LIKE folds
;;;;     ASCII letters only (as SQLite): (like :name "b%") finds "Bert",
;;;;     (like :name "ä%") does not find "Ärger".
;;;;   - Sorting as Nushell: stable, NULL last; :desc reverses the whole order
;;;;     (NULL first, ties reversed), as sort-by -r.
;;;;   - The same pipe gives the same rows in memory, in SQLite and in Nushell;
;;;;     where Nushell cannot, cases.sexp says so.

(:vocabulary 1

 (:word where :args (condition) :nushell "where" :elisp qss-where :cl where :status :both
  :meaning "Keep the rows for which CONDITION is true.")
 (:word filter :args (condition) :nushell "filter" :elisp qss-filter :cl filter :status :both
  :meaning "Another name of where.")
 (:word select :args (&rest column) :nushell "select" :elisp qss-select :cl select :status :both
  :meaning "Only these columns, in this order. CL also: (column :as :name).")
 (:word reject :args (&rest column) :nushell "reject" :elisp qss-reject :cl reject :status :both
  :meaning "All columns except these.")
 (:word get :args (column) :nushell "get" :elisp qss-get :cl get-column :status :both
  :meaning "The values of one column; collect gives the values, not rows.")
 (:word sort-by :args (&rest column-and-direction) :nushell "sort-by" :elisp qss-sort-by :cl sort-by
  :status :both
  :meaning "Sort by the columns, each followed by :asc (default) or :desc: (sort-by :age :desc :nr) is
sort-by nr | sort-by age -r in Nushell. Stable, NULL last; :desc reverses the whole order.")
 (:word first :args (&optional n) :nushell "first" :elisp qss-first :cl take :status :both
  :meaning "The first N rows. Without N the first row itself, not a table (nil for no rows). CL also: take, limit.")
 (:word head :args (&optional n) :nushell nil :elisp qss-head :cl head :status :both
  :meaning "The first N rows, default 10 (from clsh; Nushell says first).")
 (:word last :args (&optional n) :nushell "last" :elisp qss-last :cl take-last :status :both
  :meaning "The last N rows, in their order. Without N the last row itself.")
 (:word join :args (other &key on kind suffix prefix) :nushell "join" :elisp qss-join :cl join
  :status :both
  :meaning "Rows of both tables whose join columns are equal (NULL matches NULL, as in Nushell).
:on a column both have, (left right), or a list of these (default: all columns both have); CL also
a condition. :kind :inner (default) or :left. A name both tables have gets _ on the right
(name_); :suffix or :prefix renames all columns of OTHER except a join column both have.")
 (:word group-by :args (&rest column) :nushell "group-by" :elisp qss-group-by :cl group-by
  :status :both
  :meaning "As Nushell's group-by --to-table: one row per group, the key columns and :items, the
rows of the group; groups in the order their keys first appear, NULL too.")
 (:word group-sum :args (&rest column-or-aggregate) :nushell nil :elisp qss-group-sum :cl group-sum
  :status :both
  :meaning "One row per group: the keys, :count and aggregates (sum :amount) (max :date :as :last);
sorted by the keys, NULL last. As SQL GROUP BY.")
 (:word summarize :args (&rest aggregate) :nushell nil :elisp qss-summarize :cl summarize :status :both
  :meaning "One row: :count and the aggregates over all rows.")
 (:word count :args () :nushell "length" :elisp qss-count :cl count-rows :status :both :ends t
  :meaning "The number of rows.")
 (:word length :args () :nushell "length" :elisp qss-length :cl count-rows :status :both :ends t
  :meaning "Another name of count, as in Nushell.")
 (:word columns :args () :nushell "columns" :elisp qss-columns-of :cl columns-of :status :both :ends t
  :meaning "The column names.")
 (:word collect :args () :nushell nil :elisp qss-collect :cl collect :status :both :ends t
  :meaning "The rows as a list of property lists (after get: the values).")
 (:word each :args (form) :nushell "each" :elisp qss-each :cl nil :status :elisp
  :meaning "A new row (or value) from each row; IT is the row.")
 (:word to :args (format) :nushell "to" :elisp qss-to :cl nil :status :elisp :ends t
  :meaning "The table as text: text org md csv tsv.")
 (:word into :args (target &rest args) :nushell nil :elisp qss-into :cl nil :status :elisp
  :meaning "Store the table, e.g. (into sqlite \"Name\").")
 (:word rename :args (&rest old-new) :nushell "rename" :elisp nil :cl nil :status :planned
  :meaning "New names for columns: (rename :ort :city)."))
