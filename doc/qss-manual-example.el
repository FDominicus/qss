;;; qss-manual-example.el --- The worked example of the qss manual  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A row type of one's own: the Emacs Lisp libraries on `load-path'.
;; The manual shows this file piece by piece; qss-manual-test.el checks
;; that it works.
;;
;;   (qss-> (libraries "^subr") (sort-by size :desc))

;;; Code:

(require 'qss)

;;; 1. The row object

(cl-defstruct (my-library (:constructor my-library--make) (:copier nil))
  "An Emacs Lisp library: its NAME (without .el), its PATH and SIZE."
  name path size)

;;; 2. The source

(defun my-libraries (&optional regexp)
  "Return a table of the Emacs Lisp libraries on `load-path'.
With REGEXP, only those whose name matches it."
  (let ((seen (make-hash-table :test #'equal)) (rows nil))
    (dolist (dir load-path)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.el\\(?:\\.gz\\)?\\'" t))
          (let ((name (replace-regexp-in-string "\\.el\\(?:\\.gz\\)?\\'" ""
                                                (file-name-nondirectory file))))
            (when (and (not (gethash name seen))
                       (or (null regexp) (string-match-p regexp name)))
              (puthash name t seen)
              (push (my-library--make
                     :name name :path file
                     :size (file-attribute-size (file-attributes file)))
                    rows))))))
    (qss-make-table (if regexp (format "libraries %s" regexp) "libraries")
                    (nreverse rows) nil
                    (lambda () (my-libraries regexp)))))

;; A short name for pipes: (libraries "^subr")
(setf (alist-get 'libraries qss-sources) 'my-libraries)

;;; 3. The protocol: columns, fields, RET, a

(cl-defmethod qss-columns ((_ my-library))
  "Return the columns of a library row."
  '((name :width 30) (size :type size) path))

(cl-defmethod qss-field ((row my-library) name)
  "Return field NAME of the library ROW."
  (pcase name
    ('name (my-library-name row))
    ('size (my-library-size row))
    ('path (my-library-path row))
    ('compiled (let ((el (string-remove-suffix ".gz" (my-library-path row))))
                 (file-exists-p (concat (file-name-sans-extension el) ".elc"))))))

(cl-defmethod qss-default-action ((row my-library))
  "Visit the library ROW."
  (find-file (my-library-path row)))

(cl-defmethod qss-actions ((row my-library))
  "Return the actions of the library ROW."
  (ignore row)
  (list (cons "Visit" #'qss-default-action)
        (cons "Load" (lambda (r)
                       (load (my-library-path r))
                       (message "Loaded %s" (my-library-name r))))
        (cons "Describe" #'qss-describe)))

;;; 4. Presentations: a library is a file

(defun my-library-named (text)
  "Return the library called TEXT, or nil."
  (car (qss-table-rows
        (my-libraries (concat "\\`" (regexp-quote text) "\\'")))))

(qss-define-presentation-type 'library 'file #'my-library-named
  "An Emacs Lisp library on `load-path'; its name can be typed.")

(cl-defmethod qss-presentation-type ((_ my-library))
  "A library row is a `library', and so a `file'."
  'library)

(cl-defmethod qss-presentation-label ((row my-library))
  "Return the name of the library ROW."
  (my-library-name row))

(cl-defmethod qss-file-name ((row my-library))
  "Return the file of the library ROW, for the commands on files."
  (my-library-path row))

;;; 5. Storing: keep the path, so a stored row still opens

(cl-defmethod qss-stored-fields ((_ my-library))
  "A stored library keeps its path (it is a column already) and nothing more."
  nil)

;;; 6. A command of its own, on libraries only

(qss-define-command my-library-requires "Show what it requires"
    ((library library "Library: "))
  "Show the features LIBRARY requires, as a table."
  (qss-display
   (qss-make-table (format "requires of %s" (qss-presentation-label library))
                   (with-temp-buffer
                     (insert-file-contents (qss-file-name library))
                     (let (found)
                       (while (re-search-forward "^(require '\\([^) \n]+\\)" nil t)
                         (push (list :feature (match-string 1)) found))
                       (nreverse found)))
                   '(feature))))

;;; 7. A contract for the source

(qss-define-contract my-libraries (-> (opt stringp) qss-table-p))

(provide 'qss-manual-example)
;;; qss-manual-example.el ends here
