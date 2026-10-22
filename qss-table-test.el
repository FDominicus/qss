;;; qss-table-test.el --- Tests for qss-table  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with:

;;   emacs --batch -L . -l qss-table-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss-table)

(defmacro qss-test--with-dir (var &rest body)
  "Bind VAR to a directory with three files and a subdirectory, then run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "qss-test-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "small.txt" ,var) (insert "one\n"))
           (with-temp-file (expand-file-name "large.txt" ,var)
             (dotimes (i 2000) (insert (format "line %d\n" i))))
           (with-temp-file (expand-file-name "empty.txt" ,var))
           (make-directory (expand-file-name "sub" ,var))
           ,@body)
       (delete-directory ,var t))))

(defun qss-test--names (table)
  (sort (mapcar (lambda (r) (qss-field r 'name)) (qss-table-rows table)) #'string<))

;;; The value

(ert-deftest qss-plist-rows ()
  (let ((tab (qss-make-table "t" (list '(:pid 1 :name "init") '(:pid 2 :name "x")))))
    (should (equal (mapcar #'car (qss-table-columns tab)) '(pid name)))
    (should (equal (qss-field (car (qss-table-rows tab)) 'name) "init"))
    (should (null (qss-field (car (qss-table-rows tab)) 'fehlt)))))

(ert-deftest qss-alist-rows ()
  (let ((row '((a . 1) (b . "zwei"))))
    (should (equal (qss-columns row) '(a b)))
    (should (equal (qss-field row 'b) "zwei"))))

(ert-deftest qss-column-defaults ()
  (should (eq 'right (plist-get (cdr (qss--column-spec '(n :type number))) :align)))
  (should (eq 'left (plist-get (cdr (qss--column-spec 'name)) :align)))
  (should (eq #'qss-format-size (plist-get (cdr (qss--column-spec '(s :type size))) :format))))

(ert-deftest qss-formats ()
  (should (equal (qss-format-size 500) "500"))
  (should (equal (qss-format-size 10240) "10k"))
  (should (equal (qss-format-size nil) ""))
  (should (equal (qss-format-value nil) ""))
  (should (equal (qss-format-value 2.25) "2.25"))
  (should (equal (qss-format-value 39.987) "39.987"))   ; not rounded
  (should (equal (qss-format-percent 2.25) "2.2"))
  (should (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\'"
                          (qss-format-time (current-time)))))

;;; Files

(ert-deftest qss-files-rows ()
  (qss-test--with-dir dir
    (let ((tab (qss-files dir)))
      (should (equal (qss-test--names tab) '("empty.txt" "large.txt" "small.txt" "sub")))
      (should (equal (mapcar #'car (qss-table-columns tab))
                     '(name type size modified mode user)))
      (let ((sub (cl-find "sub" (qss-table-rows tab)
                          :key (lambda (r) (qss-field r 'name)) :test #'equal))
            (large (cl-find "large.txt" (qss-table-rows tab)
                            :key (lambda (r) (qss-field r 'name)) :test #'equal)))
        (should (qss-field sub 'dir-p))
        (should (equal (qss-field sub 'type) "dir"))
        (should (equal (qss-field large 'type) "file"))
        (should (> (qss-field large 'size) 10000))
        (should (file-equal-p (qss-field large 'path) (expand-file-name "large.txt" dir)))))))

(ert-deftest qss-files-empty-dir ()
  (let ((dir (make-temp-file "qss-empty-" t)))
    (unwind-protect
        (let ((tab (qss-files dir)))
          (should (null (qss-table-rows tab)))
          (should (qss-table-columns tab))           ; columns even without rows
          (with-current-buffer (qss-show tab)
            (should (string-match-p "no rows" (buffer-string)))
            (kill-buffer)))
      (delete-directory dir t))))

(ert-deftest qss-files-symlink ()
  (qss-test--with-dir dir
    (make-symbolic-link "small.txt" (expand-file-name "link-to-small" dir))
    (let ((z (cl-find "link-to-small" (qss-table-rows (qss-files dir))
                      :key (lambda (r) (qss-field r 'name)) :test #'equal)))
      (should (equal (qss-field z 'type) "link"))
      (should (equal (qss-field z 'target) "small.txt")))))

;;; Processes

(ert-deftest qss-processes-has-self ()
  (let* ((tab (qss-processes))
         (me (cl-find (emacs-pid) (qss-table-rows tab) :key #'qss-process-pid)))
    (should me)
    (should (integerp (qss-field me 'pid)))
    (should (> (qss-field me 'rss) 1024))          ; in bytes, not KB
    (should (string-match-p "emacs" (qss-field me 'command)))
    (should (stringp (qss-field me 'user)))))

;;; Display

(ert-deftest qss-show-table ()
  (qss-test--with-dir dir
    (with-current-buffer (qss-show (qss-files dir))
      (unwind-protect
          (let ((text (buffer-string)))
            (should (eq major-mode 'qss-table-mode))
            (should (string-match-p "small.txt" text))
            (should (string-match-p "large.txt" text))
            ;; size formatted, not the raw number
            (should (string-match-p " [0-9.]+k " text))
            ;; the row at point is an object
            (goto-char (point-min))
            (search-forward "small.txt")
            (should (qss-file-p (qss-current-row)))
            (should (equal (qss-field (qss-current-row) 'name) "small.txt")))
        (kill-buffer)))))

(ert-deftest qss-sort-by-size ()
  "vtable sorts the size as a number, not as text."
  (qss-test--with-dir dir
    (with-current-buffer (qss-show (qss-files dir))
      (unwind-protect
          (let ((vt (save-excursion (goto-char (point-min)) (search-forward "small")
                                    (vtable-current-table))))
            (setf (vtable-sort-by vt) (list (cons 2 'descend)))
            (save-excursion (goto-char (point-min)) (search-forward "small")
                            (vtable-revert))
            (goto-char (point-min))
            (let ((g (search-forward "large.txt"))
                  (k (progn (goto-char (point-min)) (search-forward "small.txt"))))
              (should (< g k))))
        (kill-buffer)))))

(ert-deftest qss-default-action-dir-and-refresh ()
  (qss-test--with-dir dir
    (let ((outer (qss-show (qss-files dir))))
      (unwind-protect
          (with-current-buffer outer
            (goto-char (point-min))
            (search-forward "sub")
            (let ((inner (qss-default-action (qss-current-row))))
              (with-current-buffer inner
                (should (string-match-p "sub/" (qss-table-title qss--table)))
                (should (string-match-p "no rows" (buffer-string)))
                ;; create a file, g: it appears
                (with-temp-file (expand-file-name "sub/new.txt" dir))
                (qss-refresh)
                (should (string-match-p "new.txt" (buffer-string)))
                (kill-buffer))))
        (kill-buffer outer)))))

(ert-deftest qss-default-action-file-opens ()
  (qss-test--with-dir dir
    (with-current-buffer (qss-show (qss-files dir))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (search-forward "small.txt")
            (qss-default-action (qss-current-row))
            (should (equal (buffer-name) "small.txt"))
            (should (equal (buffer-string) "one\n"))
            (kill-buffer))
        (kill-buffer (get-buffer (format "*qss files %s*"
                                         (abbreviate-file-name (file-name-as-directory dir)))))))))

(ert-deftest qss-describe-shows-fields ()
  (qss-test--with-dir dir
    (let ((row (car (qss-table-rows (qss-files dir)))))
      (qss-describe row)
      (with-current-buffer (help-buffer)
        (should (string-match-p "^modified +[0-9]\\{4\\}-" (buffer-string)))
        (should (string-match-p "^type " (buffer-string)))))))

(ert-deftest qss-keys-on-row ()
  "RET and a are bound on a row, g in the whole buffer."
  (qss-test--with-dir dir
    (with-current-buffer (qss-show (qss-files dir))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (search-forward "small.txt")
            (should (key-binding (kbd "RET")))
            (should (key-binding (kbd "a")))
            (should (eq (key-binding (kbd "g")) #'qss-refresh))
            ;; action menu: the choice runs
            (let ((called nil))
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Copy path")))
                (qss--actions-menu (qss-current-row))
                (setq called (car kill-ring)))
              (should (string-suffix-p "small.txt" called))))
        (kill-buffer)))))

(ert-deftest qss-widths-from-display ()
  "Widths come from the displayed text, not the raw value.
Long texts are cut without vtable measuring them."
  (let* ((long (make-string 50000 ?x))
         (rows (list (list :n 1 :when (current-time) :text long)
                     (list :n 2 :when (current-time) :text "kurz")))
         (tab (qss-make-table "breiten" rows '((n :type number) (when :type time) text))))
    (should (= 16 (qss--column-width tab (nth 1 (qss-table-columns tab))))) ; 2026-10-22 07:11
    (should (= qss-max-column-width (qss--column-width tab (nth 2 (qss-table-columns tab)))))
    (let ((start (float-time)))
      (with-current-buffer (qss-show tab)
        (unwind-protect
            (progn
              (should (< (- (float-time) start) 2.0))
              (should (< (buffer-size) 1000))
              (should (string-match-p "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]"
                                      (buffer-string))))
          (kill-buffer))))))

(ert-deftest qss-processes-show-fast ()
  (let ((start (float-time)))
    (with-current-buffer (qss-show (qss-processes))
      (unwind-protect
          (should (< (- (float-time) start) 3.0))
        (kill-buffer)))))

(provide 'qss-table-test)
;;; qss-table-test.el ends here
