;;; qss-interface.el --- The interface of qss, with contracts  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; The public interface of qss in one place, every function with its
;; contract (see qss-contract.el), as provide/contract in Racket: what
;; it takes, what it returns.  Read it as the reference of qss; Emacs
;; checks it on every call, and C-h f shows it.
;;
;; A broken contract names the guilty party:
;;
;;   qss-columns: the result must satisfy (listof qss-column-spec),
;;   got (name 42) (element 1 is 42, not qss-column-spec)
;;     blame: qss-columns itself, the method for my-thing
;;
;; The protocol of row types comes first, since own methods are where
;; mistakes happen.  Functions that run once per row or per cell have
;; no contract on purpose (`qss-field', the formats of values; see
;; below): the contracts stay at the boundaries, where they cost
;; nothing noticeable.
;;
;; (contracts) is a source: (qss-> (contracts) (where (like :function
;; "%sql%"))).  `qss-contract-enabled' turns the checks off.

;;; Code:

(require 'cl-lib)
(require 'qss-contract)
(require 'qss-table)
(require 'qss-pipe)
(require 'qss-sql)
(require 'qss-nu)
(require 'qss-nuon)
(require 'qss-present)
(require 'qss-menu)
(require 'qss-edit)

;;; ------------------------------------------------------------------
;;; Named contracts

(defun qss-interface--column-plist-p (plist)
  "Return non-nil if PLIST is the property list of a column spec."
  (and (proper-list-p plist)
       (cl-evenp (length plist))
       (cl-loop for (k v) on plist by #'cddr
                always (and (keywordp k)
                            (or (not (eq k :type))
                                (memq v '(nil number size duration time)))))))

(qss-define-contract-type 'qss-column-spec
  '(or/c symbolp (cons/c symbolp qss-interface--column-plist-p))
  "A column: a field name, or (NAME . PLIST) with :type, :format, :align, :width.")

(qss-define-contract-type 'qss-column-name '(or/c symbolp stringp)
  "A column name: name, :name or \"name\".")

(qss-define-contract-type 'qss-tabular '(or/c qss-table-p qss-relation-p list recordp)
  "What the words take: a table, a query, a list of rows, or one row (an object).")

(qss-define-contract-type 'qss-result '(or/c qss-table-p qss-relation-p)
  "What most words return: a table, or a query on SQLite.")

(qss-define-contract-type 'qss-aggregate
  '(and/c consp (lambda (a) (memq (car a) qss-aggregates)))
  "An aggregate: (FN COLUMN) or (FN COLUMN :as NAME), FN one of sum avg min max count.")

(qss-define-contract-type 'qss-action '(cons/c stringp functionp)
  "An action: (LABEL . FUNCTION).")

(qss-define-contract-type 'qss-presentation-type-name 'qss-presentation-type-p
  "A presentation type: file, directory, size, value ... or t.")

(qss-define-contract-type 'qss-format '(or/c (one-of text org md csv tsv) stringp)
  "A text format of `qss-to'.")

;;; ------------------------------------------------------------------
;;; The protocol of row types (methods: `cl-defmethod')

(qss-define-contract qss-columns (-> any (listof qss-column-spec)))
(qss-define-contract qss-actions (-> any (listof qss-action)))
(qss-define-contract qss-stored-fields (-> any (listof symbolp)))
(qss-define-contract qss-presentation-type (-> any qss-presentation-type-name))
(qss-define-contract qss-presentation-label (-> any stringp))

;;; ------------------------------------------------------------------
;;; Tables and sources

(qss-define-contract qss-make-table
  (-> stringp list (opt (listof qss-column-spec)) (opt functionp) qss-table-p))
(qss-define-contract qss-files (-> (opt stringp) qss-table-p))
(qss-define-contract qss-processes (-> qss-table-p))
(qss-define-contract qss-run (-> stringp (rest stringp) qss-table-p))
(qss-define-contract qss-sqlite (-> stringp (opt (or/c stringp symbolp)) qss-relation-p))
(qss-define-contract qss-odbc-source
  (-> stringp (opt (or/c stringp symbolp)) (or/c qss-relation-p qss-table-p)))
(qss-define-contract qss-sql-relation (-> any (or/c stringp symbolp) qss-relation-p))
(qss-define-contract qss-tables (-> (opt stringp) qss-table-p))
(qss-define-contract qss-nu (-> stringp any))
(qss-define-contract qss-demo-database (-> stringp))
(qss-define-contract qss-as-table (-> any qss-table-p))
(qss-define-contract qss-as-input (-> any qss-result))
(qss-define-contract qss-show (-> qss-table-p (opt any) bufferp))

;;; ------------------------------------------------------------------
;;; The words

(qss-define-contract qss-where (-> qss-tabular (or/c qss-condition-p functionp) qss-result))
(qss-define-contract qss-select (-> qss-tabular (rest qss-column-name) qss-result))
(qss-define-contract qss-reject (-> qss-tabular (rest qss-column-name) qss-result))
(qss-define-contract qss-sort-by (-> qss-tabular (rest qss-column-name) qss-result))
(qss-define-contract qss-first (-> qss-tabular (opt natnump) any))
(qss-define-contract qss-last (-> qss-tabular (opt natnump) any))
(qss-define-contract qss-get (-> qss-tabular qss-column-name any))
(qss-define-contract qss-group-by (-> qss-tabular (rest qss-column-name) qss-result))
(qss-define-contract qss-group-sum (-> qss-tabular (rest (or/c qss-column-name qss-aggregate)) qss-result))
(qss-define-contract qss-summarize (-> qss-tabular (rest qss-aggregate) qss-result))
(qss-define-contract qss-join (-> qss-tabular qss-tabular (rest any) qss-result))
(qss-define-contract qss-each (-> qss-tabular functionp qss-result))
(qss-define-contract qss-columns-of (-> qss-tabular qss-table-p))
(qss-define-contract qss-count (-> qss-tabular natnump))
(qss-define-contract qss-collect (-> any any))
(qss-define-contract qss-to (-> qss-tabular qss-format stringp))
(qss-define-contract qss-into (-> qss-tabular symbolp (rest any) any))
(qss-define-contract qss-nu-pipe (-> qss-tabular stringp any))

;;; ------------------------------------------------------------------
;;; SQLite

(qss-define-contract qss-sql (-> any stringp))
(qss-define-contract qss-sql-schema (-> stringp (listof (cons/c stringp list))))
(qss-define-contract qss-sql-write
  (-> qss-table-p stringp stringp (opt (one-of replace append)) natnump))
(qss-define-contract qss-sql-name-key (-> stringp symbolp))
(qss-define-contract qss-sql-rowid (-> any (or/c null integerp)))
(qss-define-contract qss-update-rows (-> qss-tabular (rest any) natnump))
(qss-define-contract qss-delete-rows (-> qss-tabular (rest any) natnump))

;;; ------------------------------------------------------------------
;;; Values as text
;;
;; Without contracts on purpose, because they run once per row or per
;; cell: `qss-format-size', `qss-format-duration', `qss-format-time'
;; (display), `qss-parse-quantity' (every comparison with "10kb"),
;; `qss-nuon-encode' (every value sent to Nushell).  A contract there
;; doubled the time of a pipe over 20000 rows.

(qss-define-contract qss-nuon-read (-> stringp any))

;;; ------------------------------------------------------------------
;;; Presentations, accept, actions

(qss-define-contract qss-define-presentation-type
  (-> symbolp qss-presentation-type-name (opt functionp) (opt stringp) symbolp))
(qss-define-contract qss-presentation-subtype-p
  (-> qss-presentation-type-name qss-presentation-type-name any))
(qss-define-contract qss-presentation-parse (-> qss-presentation-type-name stringp any))
(qss-define-contract qss-present (-> any (opt qss-presentation-type-name) (opt stringp) any))
(qss-define-contract qss-accept (-> qss-presentation-type-name (opt stringp) any))
(qss-define-contract qss-file-name (-> any stringp))
(qss-define-contract qss-commands-for
  (-> any qss-presentation-type-name (listof qss-action)))
(qss-define-contract qss-actions-at-point (-> (listof (listof qss-action))))
(qss-define-contract qss-continue (-> qss-tabular consp any))
(qss-define-contract qss-copy-table-as (-> qss-tabular qss-format any))

;;; ------------------------------------------------------------------
;;; The contracts as a table

(defun qss-contracts ()
  "Return a table of the functions with a contract and their contracts."
  (qss-make-table
   "contracts"
   (mapcar (lambda (fn)
             (list :function (symbol-name fn)
                   :kind (if (qss-contract--generic-p fn) "generic" "function")
                   :contract (prin1-to-string (qss-contract-of fn))))
           (qss-contract-functions))
   '((function :width 32) kind (contract :width 80))
   #'qss-contracts))

(setf (alist-get 'contracts qss-sources) 'qss-contracts)

(provide 'qss-interface)
;;; qss-interface.el ends here
