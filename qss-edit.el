;;; qss-edit.el --- Changing rows of SQLite tables  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;; This file is part of qss.  See qss.el for the package headers and
;; LICENSE for the license text.

;;; Commentary:

;; SQLite as a file to work in, not only to read and to store whole
;; tables in.  Two ways:
;;
;; In a table buffer of an SQLite table (`[buffer]' in the REPL, or
;; `qss-display'):
;;
;;   e        change the cell at point (with C-u: make it empty, NULL)
;;   d        mark the row for deletion (again: unmark)
;;   u        forget the changes of the row
;;   C-c C-c  write all changes, in one transaction
;;   C-c C-k  forget all changes
;;
;; Changes are shown where they are made and written only on C-c C-c;
;; `g' and killing the buffer ask first while there are some.
;;
;; As words, for many rows at once:
;;
;;   (sqlite "big-files") (where (> :size "1gb")) (update-rows :note "archive")
;;   (sqlite "big-files") (where (is-null :path)) (delete-rows)
;;
;; They return the number of rows changed.  Without a `where' (or
;; `first') they refuse, unless :all-rows t says so.
;;
;; Rows are found by their rowid, so only queries straight on a table
;; can be changed: not views, joins or groups.

;;; Code:

(require 'cl-lib)
(require 'qss-sql)
(require 'qss-menu)

(defface qss-edit-changed '((t :inherit warning :underline t))
  "Face of a cell changed but not yet written."
  :group 'qss)

(defface qss-edit-deleted '((t :inherit shadow :strike-through t))
  "Face of a row marked for deletion."
  :group 'qss)

;;; ------------------------------------------------------------------
;;; Common: which query, which table, which values

(defun qss-edit--check-relation (rel what)
  "Signal an error unless the rows of REL can be changed; WHAT is the word."
  (unless (and (qss-relation-p rel) (stringp (qss-relation-file rel)))
    (user-error "%s works on SQLite tables only" what))
  (unless (qss-sql--direct-p rel)
    (user-error "%s: this query is not straight on a table (a join, a group or after first)" what))
  (qss-sql--with-db db (qss-relation-file rel)
    (unless (qss-sql--real-table-p db rel)
      (user-error "%s: %s is a view, not a table" what (qss-relation-table rel)))))

(defun qss-edit--column (rel key)
  "Return the column entry (KEY NAME TYPE QSS-TYPE) of KEY in REL."
  (qss-sql--column rel key)              ; signals if unknown
  (assq key (qss-relation-columns rel)))

(defun qss-edit--value (column value)
  "Return VALUE as it is stored in COLUMN (an entry of the schema).
Times become text, \"10kb\" a number in a numeric column."
  (let ((v (qss-sql--value value)))
    (if (and (stringp v) (eq (qss-sql--affinity (nth 2 column)) 'numeric))
        (qss-sql--quantity v)
      v)))

(defun qss-edit--rowids-sql (rel)
  "Return (SQL . PARAMS) selecting the rowids of the rows of REL."
  (qss-sql-query rel "rowid"))

;;; ------------------------------------------------------------------
;;; The words update-rows and delete-rows

(defun qss-edit--split-all-rows (args)
  "Return (ALL . ARGS) with :all-rows and its value taken out of ARGS."
  (let ((all nil) (rest nil))
    (while args
      (let ((a (pop args)))
        (if (eq a :all-rows) (setq all (pop args)) (push a rest) (push (pop args) rest))))
    (cons all (nreverse rest))))

(defun qss-edit--guard (rel all what)
  "Refuse WHAT on all rows of REL unless ALL says so."
  (unless (or all (qss-relation-wheres rel) (qss-relation-limit rel))
    (user-error "%s without where would change every row of %s; add :all-rows t if you mean it"
                what (qss-relation-table rel))))

(cl-defgeneric qss-update-rows (table &rest changes)
  "Set columns in the stored rows of TABLE, an SQLite query; return their number.
CHANGES: :COLUMN VALUE ...  (update-rows :status \"paid\" :paid-on \"2026-10-23\").
Only rows of the query change: its where, first and sort-by count.
Without where or first it refuses, unless :all-rows t.")

(cl-defmethod qss-update-rows (_table &rest _changes)
  "Refuse: only stored rows can be changed."
  (user-error "update-rows works on SQLite tables only; for a table in memory use each"))

(cl-defmethod qss-update-rows ((rel qss-relation) &rest changes)
  "Change the rows of REL in SQLite."
  (pcase-let ((`(,all . ,pairs) (qss-edit--split-all-rows changes)))
    (qss-edit--check-relation rel "update-rows")
    (qss-edit--guard rel all "update-rows")
    (unless pairs (user-error "update-rows: say what to set, e.g. (update-rows :status \"paid\")"))
    (let* ((sets nil) (params nil))
      (while pairs
        (let* ((key (qss--name (pop pairs)))
               (value (pop pairs))
               (column (qss-edit--column rel key)))
          (push (format "%s = ?" (qss-sql-quote (nth 1 column))) sets)
          (push (qss-edit--value column value) params)))
      (let ((ids (qss-edit--rowids-sql rel)))
        (qss-sql--with-db db (qss-relation-file rel)
          (with-sqlite-transaction db
            (sqlite-execute db (format "UPDATE %s SET %s WHERE rowid IN (\n%s\n)"
                                       (qss-sql-quote (qss-relation-table rel))
                                       (mapconcat #'identity (nreverse sets) ", ")
                                       (car ids))
                            (vconcat (nreverse params) (cdr ids)))))))))

(cl-defgeneric qss-delete-rows (table &rest options)
  "Delete the stored rows of TABLE, an SQLite query; return their number.
Only rows of the query go: its where, first and sort-by count.  Without
where or first it refuses, unless OPTIONS has :all-rows t.")

(cl-defmethod qss-delete-rows (_table &rest _options)
  "Refuse: only stored rows can be deleted."
  (user-error "delete-rows works on SQLite tables only; for a table in memory use where"))

(cl-defmethod qss-delete-rows ((rel qss-relation) &rest options)
  "Delete the rows of REL in SQLite."
  (pcase-let ((`(,all . ,rest) (qss-edit--split-all-rows options)))
    (when rest (user-error "delete-rows: the only option is :all-rows t, not %S" (car rest)))
    (qss-edit--check-relation rel "delete-rows")
    (qss-edit--guard rel all "delete-rows")
    (let ((ids (qss-edit--rowids-sql rel)))
      (qss-sql--with-db db (qss-relation-file rel)
        (with-sqlite-transaction db
          (sqlite-execute db (format "DELETE FROM %s WHERE rowid IN (\n%s\n)"
                                     (qss-sql-quote (qss-relation-table rel)) (car ids))
                          (vconcat (cdr ids))))))))

(setf (alist-get 'update-rows qss--words) 'values)
(setf (alist-get 'delete-rows qss--words) 'values)

;;; ------------------------------------------------------------------
;;; Editing in a table buffer

(defvar-local qss-edit--changes nil
  "Pending changes in this buffer: alist ROW -> (:set ALIST :delete BOOL).
ALIST is ((COLUMN . VALUE) ...); the rows are those shown.")

(defvar-local qss-edit--overlays nil
  "Overlays that show the pending changes.")

(defun qss-edit--relation ()
  "Return the SQLite table of this buffer if its rows can be changed."
  (unless (derived-mode-p 'qss-table-mode)
    (user-error "To change rows, show the table in a buffer of its own ([buffer])"))
  (let ((rel (or qss--whole (qss-table-at-point))))
    (qss-edit--check-relation rel "Editing")
    rel))

(defun qss-edit--row ()
  "Return the row at point and check that it has a rowid."
  (let ((row (qss-current-row)))
    (unless (qss-sql-rowid row)
      (user-error "This row has no rowid; read the table again with g"))
    row))

(defun qss-edit--entry (row)
  "Return the pending entry of ROW, creating it."
  (or (assq row qss-edit--changes)
      (car (push (list row :set nil :delete nil) qss-edit--changes))))

(defun qss-edit--count ()
  "Return (CHANGED . DELETED), the number of rows with pending changes."
  (cons (cl-count-if (lambda (e) (and (plist-get (cdr e) :set) (not (plist-get (cdr e) :delete))))
                     qss-edit--changes)
        (cl-count-if (lambda (e) (plist-get (cdr e) :delete)) qss-edit--changes)))

(defun qss-edit-pending-p ()
  "Return non-nil if this buffer has changes not yet written."
  (and qss-edit--changes
       (cl-some (lambda (e) (or (plist-get (cdr e) :set) (plist-get (cdr e) :delete)))
                qss-edit--changes)))

(defun qss-edit--update-mode-line ()
  "Show the number of pending changes in the mode line."
  (let ((n (qss-edit--count)))
    (setq mode-line-process
          (if (qss-edit-pending-p)
              (format " [%d changed, %d to delete; C-c C-c writes]" (car n) (cdr n))
            nil))
    (force-mode-line-update)))

(defun qss-edit--cell-bounds (row index)
  "Return (BEG . END) of the text of column INDEX in the line of ROW."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) (point-max)))
        (if (eq (get-text-property (point) 'vtable-object) row)
            (setq found t)
          (forward-line 1)))
      (when found
        (let ((end (line-end-position)) (beg nil) (last nil))
          (while (< (point) end)
            (when (and (eql (get-text-property (point) 'vtable-column) index)
                       (not (get-text-property (point) 'display)))
              (unless beg (setq beg (point)))
              (setq last (1+ (point))))
            (forward-char 1))
          (and beg (cons beg last)))))))

(defun qss-edit--redisplay ()
  "Show the pending changes with overlays."
  (mapc #'delete-overlay qss-edit--overlays)
  (setq qss-edit--overlays nil)
  (let ((vt (save-excursion
              (goto-char (point-min))
              (let ((match (text-property-search-forward 'vtable)))
                (and match (prop-match-value match))))))
    (dolist (e qss-edit--changes)
      (let ((row (car e)))
        (if (plist-get (cdr e) :delete)
            (let ((b (qss-edit--cell-bounds row 0)))
              (when b
                (let ((o (make-overlay (save-excursion (goto-char (car b)) (line-beginning-position))
                                       (save-excursion (goto-char (car b)) (line-end-position)))))
                  (overlay-put o 'face 'qss-edit-deleted)
                  (push o qss-edit--overlays))))
          (dolist (change (plist-get (cdr e) :set))
            (let* ((index (and vt (cl-position (symbol-name (car change))
                                               (mapcar #'vtable-column-name (vtable-columns vt))
                                               :test #'equal)))
                   (b (and index (qss-edit--cell-bounds row index))))
              (when b
                (let* ((width (string-width (buffer-substring (car b) (cdr b))))
                       (spec (assq (car change) (qss-table-columns qss--table)))
                       (text (if (cdr change)
                                 (funcall (or (plist-get (cdr spec) :format) #'qss-format-value)
                                          (cdr change))
                               "NULL"))
                       (shown (truncate-string-to-width
                               text width nil ?\s (if (char-displayable-p ?…) "…" "~")))
                       (o (make-overlay (car b) (cdr b))))
                  (overlay-put o 'display shown)
                  (overlay-put o 'face 'qss-edit-changed)
                  (overlay-put o 'help-echo (format "%s: %s (not written yet)" (car change) text))
                  (push o qss-edit--overlays))))))))
    (qss-edit--update-mode-line)))

(defun qss-edit--read-value (column current)
  "Read a new value for COLUMN (an entry of the schema), starting with CURRENT.
A numeric column takes numbers (and \"10kb\", \"3hr\"); empty input
there means NULL.  A text column takes the text as it is."
  (let* ((numeric (eq (qss-sql--affinity (nth 2 column)) 'numeric))
         (text (read-string (format "%s%s: " (car column) (if numeric " (empty: NULL)" ""))
                            (if current (qss--plain-value current nil) ""))))
    (cond ((not numeric) text)
          ((string-empty-p (string-trim text)) nil)
          ((let ((q (qss-parse-quantity (string-trim text)))) (and q (cdr q))))
          ((qss--parse-number text))
          (t (user-error "%s takes a number, not %S" (car column) text)))))

(defun qss-edit-cell (&optional make-null)
  "Change the cell at point; written by \\[qss-edit-commit].
With a prefix argument MAKE-NULL, make it empty (NULL)."
  (interactive "P")
  (let* ((rel (qss-edit--relation))
         (row (qss-edit--row))
         (key (or (qss--column-at-point) (user-error "No column here")))
         (column (qss-edit--column rel key))
         (entry (qss-edit--entry row))
         (pending (assq key (plist-get (cdr entry) :set)))
         (current (if pending (cdr pending) (qss-field row key)))
         (value (if make-null nil (qss-edit--read-value column current))))
    (setf (cdr entry) (plist-put (cdr entry) :set
                                 (cons (cons key value)
                                       (assq-delete-all key (plist-get (cdr entry) :set)))))
    (qss-edit--redisplay)))

(defun qss-edit-toggle-delete ()
  "Mark the row at point for deletion, or unmark it."
  (interactive)
  (qss-edit--relation)
  (let* ((row (qss-edit--row))
         (entry (qss-edit--entry row)))
    (setf (cdr entry) (plist-put (cdr entry) :delete (not (plist-get (cdr entry) :delete))))
    (qss-edit--redisplay)
    (forward-line 1)))

(defun qss-edit-undo-row ()
  "Forget the pending changes of the row at point."
  (interactive)
  (let ((row (qss-current-row)))
    (setq qss-edit--changes (assq-delete-all row qss-edit--changes))
    (qss-edit--redisplay)))

(defun qss-edit-discard ()
  "Forget all pending changes of this buffer."
  (interactive)
  (setq qss-edit--changes nil)
  (qss-edit--redisplay)
  (message "Changes forgotten"))

(defun qss-edit-commit ()
  "Write the pending changes of this buffer in one transaction, then read again.
If one row cannot be written (it was deleted meanwhile, say), nothing is."
  (interactive)
  (let ((rel (qss-edit--relation))
        (changed 0) (deleted 0))
    (unless (qss-edit-pending-p) (user-error "No changes"))
    (qss-sql--with-db db (qss-relation-file rel)
      (with-sqlite-transaction db
        (dolist (e (reverse qss-edit--changes))
          (let ((id (qss-sql-rowid (car e)))
                (table (qss-sql-quote (qss-relation-table rel))))
            (cond
             ((plist-get (cdr e) :delete)
              (unless (eql 1 (sqlite-execute db (format "DELETE FROM %s WHERE rowid = ?" table)
                                             (vector id)))
                (error "Row %s is gone; nothing written" id))
              (cl-incf deleted))
             ((plist-get (cdr e) :set)
              (let ((sets (reverse (plist-get (cdr e) :set))))
                (unless (eql 1 (sqlite-execute
                                db (format "UPDATE %s SET %s WHERE rowid = ?" table
                                           (mapconcat (lambda (s)
                                                        (format "%s = ?" (qss-sql-quote
                                                                          (nth 1 (qss-edit--column rel (car s))))))
                                                      sets ", "))
                                (vconcat (mapcar (lambda (s) (qss-edit--value (qss-edit--column rel (car s))
                                                                              (cdr s)))
                                                 sets)
                                         (list id))))
                  (error "Row %s is gone; nothing written" id))
                (cl-incf changed))))))))
    (setq qss-edit--changes nil)
    (qss-edit--redisplay)
    (qss-refresh)
    (message "Written: %d changed, %d deleted" changed deleted)))

(defun qss-edit--confirm-discard ()
  "Ask before pending changes are lost; return non-nil to go on."
  (or (not (qss-edit-pending-p))
      (yes-or-no-p (format "%s has changes not written; forget them? " (buffer-name)))))

(defun qss-edit--before-refresh ()
  "Called by `qss-refresh': ask about pending changes, then forget them."
  (unless (qss-edit--confirm-discard) (user-error "Not read again; C-c C-c writes the changes"))
  (setq qss-edit--changes nil)
  (mapc #'delete-overlay qss-edit--overlays)
  (setq qss-edit--overlays nil)
  (setq mode-line-process nil))

(defun qss-edit--kill-buffer-query ()
  "Ask before a buffer with pending changes is killed."
  (qss-edit--confirm-discard))

(defun qss-edit--setup ()
  "Set up a table buffer for editing."
  (add-hook 'kill-buffer-query-functions #'qss-edit--kill-buffer-query nil t))

(add-hook 'qss-table-mode-hook #'qss-edit--setup)

(define-key qss-table-mode-map (kbd "C-c C-c") #'qss-edit-commit)
(define-key qss-table-mode-map (kbd "C-c C-k") #'qss-edit-discard)

(provide 'qss-edit)
;;; qss-edit.el ends here
