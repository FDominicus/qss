;;; qss-edit-test.el --- Tests for changing rows of SQLite tables  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-edit-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defmacro qss-edtest--with-db (var &rest body)
  "Bind VAR to a fresh demo database, then run BODY."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let* ((temporary-file-directory (file-name-as-directory (make-temp-file "qss-edit-" t)))
            (,var (qss-demo-database)))
       (unwind-protect (progn ,@body)
         (delete-directory temporary-file-directory t)))))

(defun qss-edtest--select (db sql)
  "Return the rows of SQL in DB, straight from SQLite."
  (qss-sql--with-db conn db (sqlite-select conn sql)))

(defmacro qss-edtest--in-buffer (rel &rest body)
  "Show REL in a table buffer and run BODY there."
  (declare (indent 1))
  `(save-window-excursion
     (let ((buf (qss-display ,rel)))
       (unwind-protect (with-current-buffer buf ,@body)
         (with-current-buffer buf (setq qss-edit--changes nil))
         (kill-buffer buf)))))

(defun qss-edtest--goto (text column)
  "Go to the cell of COLUMN in the row line containing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (while (and (not (eolp)) (not (eq (qss--column-at-point) column)))
    (forward-char 1))
  (should (eq (qss--column-at-point) column)))

;;; ------------------------------------------------------------------
;;; The words

(ert-deftest qss-edit-update-rows ()
  (qss-edtest--with-db db
    (should (= 2 (qss-> (sqlite db "Orders") (where (is-null :status)) (update-rows :status "open"))))
    (should (equal (qss-edtest--select db "SELECT OrderNo FROM Orders WHERE Status = 'open' ORDER BY 1")
                   '((4713) (4715))))
    ;; several columns; values are Lisp, evaluated
    (let ((amount 1.5))
      (should (= 1 (qss-> (sqlite db "Orders") (where (= :order-no 4711))
                          (update-rows :amount (* 2 amount) :status nil)))))
    (should (equal (qss-edtest--select db "SELECT Amount, Status FROM Orders WHERE OrderNo = 4711")
                   '((3.0 nil))))
    ;; first and sort-by count: the two largest amounts
    (should (= 2 (qss-> (sqlite db "Orders") (sort-by amount :desc) (first 2) (update-rows :status "big"))))
    (should (equal (qss-edtest--select db "SELECT OrderNo FROM Orders WHERE Status = 'big' ORDER BY 1")
                   '((4713) (4715))))))

(ert-deftest qss-edit-delete-rows ()
  (qss-edtest--with-db db
    (should (= 1 (qss-> (sqlite db "Orders") (where (= :status "cancelled")) (delete-rows))))
    (should (= 5 (qss-count (qss-sqlite db "Orders"))))
    (should (= 0 (qss-> (sqlite db "Orders") (where (> :amount 5000)) (delete-rows))))))

(ert-deftest qss-edit-guard-all-rows ()
  (qss-edtest--with-db db
    (should-error (qss-> (sqlite db "Orders") (delete-rows)) :type 'user-error)
    (should-error (qss-> (sqlite db "Orders") (update-rows :status "x")) :type 'user-error)
    (should (= 6 (qss-count (qss-sqlite db "Orders"))))
    (should (= 6 (qss-> (sqlite db "Orders") (update-rows :status "x" :all-rows t))))
    (should (= 6 (qss-> (sqlite db "Orders") (delete-rows :all-rows t))))
    (should (= 0 (qss-count (qss-sqlite db "Orders"))))))

(ert-deftest qss-edit-refuses-what-cannot-change ()
  (qss-edtest--with-db db
    (let ((qss-contract-enabled nil))
      (should-error (qss-> '((:a 1)) (delete-rows)) :type 'user-error)
      (should-error (qss-> (sqlite db "Orders") (join (sqlite db "Customers") :on customer-no)
                           (where (= :city "London")) (delete-rows))
                    :type 'user-error)
      (should-error (qss-> (sqlite db "Orders") (group-by status) (where (= :count 1)) (delete-rows))
                    :type 'user-error)
      (should-error (qss-> (sqlite db "Orders") (where (= :order-no 1)) (update-rows :no-such 1))
                    :type 'user-error)
      (qss-sql--with-db conn db (sqlite-execute conn "CREATE VIEW Paid AS SELECT * FROM Orders WHERE Status = 'paid'"))
      (should-error (qss-> (sqlite db "Paid") (where (= :order-no 4711)) (delete-rows)) :type 'user-error)
      ;; nothing changed
      (should (= 6 (qss-count (qss-sqlite db "Orders")))))))

(ert-deftest qss-edit-quantities ()
  (qss-edtest--with-db db
    (qss-> (files temporary-file-directory) (into sqlite "f" :file db))
    (should (= 1 (qss-> (sqlite db "f") (where (like :name "%.sqlite3")) (update-rows :size "2kb"))))
    (should (equal (qss-edtest--select db "SELECT size FROM f WHERE name LIKE '%.sqlite3'") '((2000))))))

;;; ------------------------------------------------------------------
;;; Editing in a buffer

(ert-deftest qss-edit-buffer-change-delete-commit ()
  (qss-edtest--with-db db
    (qss-edtest--in-buffer (qss-sqlite db "Orders")
      (qss-edtest--goto "4711" 'amount)
      (should (eq (key-binding (kbd "e")) #'qss-edit-cell))
      (cl-letf (((symbol-function 'read-string) (lambda (_p initial &rest _)
                                                  (should (equal initial "120.0"))
                                                  "125.5")))
        (qss-edit-cell))
      (qss-edtest--goto "4711" 'status)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "refund")))
        (qss-edit-cell))
      (qss-edtest--goto "4712" 'order-no)
      (qss-edit-toggle-delete)
      ;; shown, not written
      (should (qss-edit-pending-p))
      (should (string-match-p "1 changed, 1 to delete" mode-line-process))
      (should (cl-some (lambda (o) (equal (overlay-get o 'display) "125.5")) qss-edit--overlays))
      (should (cl-some (lambda (o) (eq (overlay-get o 'face) 'qss-edit-deleted)) qss-edit--overlays))
      (should (equal (qss-edtest--select db "SELECT Amount FROM Orders WHERE OrderNo = 4711") '((120.0))))
      (qss-edit-commit)
      (should-not (qss-edit-pending-p))
      (should (equal (qss-edtest--select db "SELECT Amount, Status FROM Orders WHERE OrderNo = 4711")
                     '((125.5 "refund"))))
      (should (null (qss-edtest--select db "SELECT * FROM Orders WHERE OrderNo = 4712")))
      ;; the buffer shows the table read again
      (should (= 5 (length (qss-table-rows qss--table))))
      (should (string-match-p "125\\.5 +refund" (buffer-string))))))

(ert-deftest qss-edit-buffer-null-undo-discard ()
  (qss-edtest--with-db db
    (qss-edtest--in-buffer (qss-sqlite db "Orders")
      (qss-edtest--goto "4714" 'status)
      (qss-edit-cell t)                              ; C-u e: NULL
      (qss-edtest--goto "4714" 'amount)
      ;; numeric column: empty input is NULL, a word is refused
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "lots")))
        (should-error (qss-edit-cell) :type 'user-error))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (qss-edit-cell))
      (should (equal (plist-get (cdr (car qss-edit--changes)) :set) '((amount) (status))))
      (qss-edit-undo-row)
      (should-not (qss-edit-pending-p))
      (qss-edtest--goto "4715" 'order-no)
      (qss-edit-toggle-delete)
      (qss-edit-discard)
      (should-not (qss-edit-pending-p))
      (should-not qss-edit--overlays)
      (should (= 6 (qss-count (qss-sqlite db "Orders"))))
      ;; the NULL itself, written
      (qss-edtest--goto "4714" 'status)
      (qss-edit-cell t)
      (qss-edit-commit)
      (should (equal (qss-edtest--select db "SELECT Status FROM Orders WHERE OrderNo = 4714") '((nil)))))))

(ert-deftest qss-edit-buffer-all-or-nothing ()
  "If one row cannot be written, nothing is."
  (qss-edtest--with-db db
    (qss-edtest--in-buffer (qss-sqlite db "Orders")
      (qss-edtest--goto "4711" 'status)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "changed")))
        (qss-edit-cell))
      (qss-edtest--goto "4716" 'order-no)
      (qss-edit-toggle-delete)
      ;; meanwhile someone else deletes 4716
      (qss-edtest--select db "DELETE FROM Orders WHERE OrderNo = 4716")
      (should-error (qss-edit-commit))
      (should (equal (qss-edtest--select db "SELECT Status FROM Orders WHERE OrderNo = 4711") '(("paid"))))
      (should (qss-edit-pending-p)))))

(ert-deftest qss-edit-buffer-asks-before-losing ()
  (qss-edtest--with-db db
    (qss-edtest--in-buffer (qss-sqlite db "Orders")
      (qss-edtest--goto "4711" 'order-no)
      (qss-edit-toggle-delete)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (should-error (qss-refresh) :type 'user-error)
        (should (qss-edit-pending-p))
        (should-not (qss-edit--kill-buffer-query)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (qss-refresh)
        (should-not (qss-edit-pending-p))))))

(ert-deftest qss-edit-buffer-only-where-possible ()
  (qss-edtest--with-db db
    ;; a join cannot be changed
    (qss-edtest--in-buffer (qss-> (sqlite db "Orders") (join (sqlite db "Customers") :on customer-no))
      (qss-edtest--goto "4711" 'amount)
      (should-error (qss-edit-cell) :type 'user-error)
      (should-not (assoc "Mark row for deletion" (apply #'append (qss-actions-at-point)))))
    ;; a table: the menu offers it
    (qss-edtest--in-buffer (qss-sqlite db "Orders")
      (qss-edtest--goto "4711" 'amount)
      (should (assoc "Change amount..." (apply #'append (qss-actions-at-point))))
      (should (assoc "Mark row for deletion" (apply #'append (qss-actions-at-point)))))
    ;; in the REPL: a hint to open a buffer
    (let ((qss-last nil))
      (save-window-excursion
        (with-current-buffer (qss)
          (unwind-protect
              (progn
                (goto-char (point-max))
                (insert (format "(sqlite %S \"Orders\")" db))
                (qss-repl-send)
                (search-backward "4711")
                (should-error (qss-edit-cell) :type 'user-error))
            (kill-buffer "*qss*")))))))

(provide 'qss-edit-test)
;;; qss-edit-test.el ends here
