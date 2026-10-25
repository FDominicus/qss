;;; qss-manual-reference.el --- Write the reference part of the qss manual  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The reference chapter of the manual is taken from the code itself,
;; so it cannot fall behind: options, faces, words, sources,
;; presentation types, commands and contracts.
;;
;;   emacs --batch -L . -L .. -l qss-manual-reference.el -f qss-manual-write-reference
;;
;; writes reference.tex, version.tex and the parts of the example
;; (example-N.el.txt) next to this file; qss-manual.tex includes them.
;; `qss-manual-build' does that and runs LuaLaTeX twice.

;;; Code:

(require 'qss)

(defun qss-manual--tex (text)
  "Return TEXT with the characters special to LaTeX escaped."
  (replace-regexp-in-string
   "[\\\\{}$&#^_%~<>|]"
   (lambda (c)
     (pcase c
       ("\\" "\\textbackslash{}") ("{" "\\{") ("}" "\\}") ("$" "\\$") ("&" "\\&")
       ("#" "\\#") ("^" "\\textasciicircum{}") ("_" "\\_") ("%" "\\%")
       ("~" "\\textasciitilde{}") ("<" "\\textless{}") (">" "\\textgreater{}")
       ("|" "\\textbar{}")))
   text t t))

(defun qss-manual--code (text)
  "Return TEXT as inline code."
  (concat "\\code{" (qss-manual--tex text) "}"))

(defun qss-manual--first-line (doc)
  "Return the first line of the docstring DOC, Emacs quotes turned into code."
  (let* ((line (car (split-string (or doc "") "\n")))
         (line (qss-manual--tex line)))
    ;; `foo' -> \code{foo}, "text" -> \enquote{text}
    (replace-regexp-in-string
     "\"\\([^\"]*\\)\"" "\\\\enquote{\\1}"
     (replace-regexp-in-string "`\\([^']+\\)'" "\\\\code{\\1}" line t))))

(defun qss-manual--table (spec header rows)
  "Return a longtable with column SPEC, HEADER cells and ROWS of cells."
  (concat "\\begin{longtable}{" spec "}\n\\toprule\n"
          (mapconcat #'identity header " & ") " \\\\\n\\midrule\n\\endhead\n"
          (mapconcat (lambda (r) (concat (mapconcat #'identity r " & ") " \\\\\n")) rows "")
          "\\bottomrule\n\\end{longtable}\n"))

(defun qss-manual--symbols-with-prefix (pred)
  "Return the symbols starting with qss- that satisfy PRED, sorted."
  (let (l)
    (mapatoms (lambda (s) (when (and (string-prefix-p "qss-" (symbol-name s))
                                     (not (string-match-p "--" (symbol-name s)))
                                     (funcall pred s))
                            (push s l))))
    (sort l #'string<)))

(defun qss-manual--options ()
  "Return the table of options."
  (qss-manual--table
   "@{}p{0.34\\linewidth}p{0.18\\linewidth}p{0.42\\linewidth}@{}"
   '("\\textbf{Option}" "\\textbf{Default}" "\\textbf{Meaning}")
   (mapcar (lambda (s)
             (list (qss-manual--code (symbol-name s))
                   ;; a file name may break after a slash, a regexp after \|
                   (replace-regexp-in-string
                    "/\\|\\\\textbar{}" "\\&\\\\allowbreak{}"
                    (qss-manual--code
                     (let ((v (eval (car (get s 'standard-value)) t)))
                       (if (and (stringp v) (string-prefix-p (expand-file-name "~") v))
                           (abbreviate-file-name v)
                         (prin1-to-string v)))))
                   (qss-manual--first-line (documentation-property s 'variable-documentation t))))
           (qss-manual--symbols-with-prefix #'custom-variable-p))))

(defun qss-manual--faces ()
  "Return the table of faces."
  (qss-manual--table
   "@{}p{0.34\\linewidth}p{0.62\\linewidth}@{}"
   '("\\textbf{Face}" "\\textbf{Used for}")
   (mapcar (lambda (f) (list (qss-manual--code (symbol-name f))
                             (qss-manual--first-line (face-documentation f))))
           (qss-manual--symbols-with-prefix #'facep))))

(defun qss-manual--words ()
  "Return the table of words with the function behind each."
  (qss-manual--table
   "@{}p{0.16\\linewidth}p{0.26\\linewidth}p{0.52\\linewidth}@{}"
   '("\\textbf{Word}" "\\textbf{Function}" "\\textbf{Meaning}")
   (mapcar (lambda (w)
             (let ((fn (qss--word-function (car w))))
               (list (qss-manual--code (symbol-name (car w)))
                     (qss-manual--code (symbol-name fn))
                     (qss-manual--first-line (documentation fn t)))))
           (sort (copy-sequence qss--words)
                 (lambda (a b) (string< (car a) (car b)))))))

(defun qss-manual--sources ()
  "Return the table of sources."
  (qss-manual--table
   "@{}p{0.16\\linewidth}p{0.26\\linewidth}p{0.52\\linewidth}@{}"
   '("\\textbf{Source}" "\\textbf{Function}" "\\textbf{Meaning}")
   (mapcar (lambda (s)
             (list (qss-manual--code (symbol-name (car s)))
                   (qss-manual--code (symbol-name (cdr s)))
                   (qss-manual--first-line (documentation (cdr s) t))))
           (sort (copy-sequence qss-sources) (lambda (a b) (string< (car a) (car b)))))))

(defun qss-manual--presentation-types ()
  "Return the table of presentation types."
  (let (rows)
    (maphash (lambda (type plist)
               (push (list (qss-manual--code (symbol-name type))
                           (qss-manual--code (format "%s" (plist-get plist :supertype)))
                           (if (plist-get plist :parser) "yes" "")
                           (qss-manual--first-line (plist-get plist :doc)))
                     rows))
             qss--presentation-types)
    (qss-manual--table
     "@{}p{0.15\\linewidth}p{0.15\\linewidth}p{0.08\\linewidth}p{0.54\\linewidth}@{}"
     '("\\textbf{Type}" "\\textbf{Below}" "\\textbf{Text}" "\\textbf{Meaning}")
     (sort rows (lambda (a b) (string< (car a) (car b)))))))

(defun qss-manual--commands ()
  "Return the table of commands with typed arguments."
  (qss-manual--table
   "@{}p{0.27\\linewidth}p{0.25\\linewidth}p{0.42\\linewidth}@{}"
   '("\\textbf{Command}" "\\textbf{In the menu}" "\\textbf{Arguments}")
   (mapcar (lambda (c)
             (list (qss-manual--code (symbol-name (car c)))
                   (qss-manual--tex (nth 1 c))
                   (mapconcat (lambda (a) (qss-manual--code (format "%s %s" (car a) (nth 1 a))))
                              (nth 2 c) ", ")))
           (sort (copy-sequence qss-commands) (lambda (a b) (string< (car a) (car b)))))))

(defun qss-manual--contract-types ()
  "Return the table of named contracts."
  (qss-manual--table
   "@{}p{0.3\\linewidth}p{0.64\\linewidth}@{}"
   '("\\textbf{Named contract}" "\\textbf{Contract and meaning}")
   (mapcar (lambda (s)
             (list (qss-manual--code (symbol-name s))
                   (concat (qss-manual--code (prin1-to-string (car (get s 'qss-contract-type))))
                           "\\newline "
                           (qss-manual--first-line (get s 'qss-contract-type-doc)))))
           (qss-manual--symbols-with-prefix (lambda (s) (get s 'qss-contract-type))))))

(defun qss-manual--contracts ()
  "Return the table of contracts."
  (qss-manual--table
   "@{}p{0.3\\linewidth}p{0.64\\linewidth}@{}"
   '("\\textbf{Function}" "\\textbf{Contract}")
   (mapcar (lambda (fn)
             (list (concat (qss-manual--code (symbol-name fn))
                           (if (qss-contract--generic-p fn) "\\newline\\emph{generic}" ""))
                   (qss-manual--code (prin1-to-string (qss-contract-of fn)))))
           (qss-contract-functions))))

(defun qss-manual--version ()
  "Return the version of qss from the header of qss.el."
  (with-temp-buffer
    (insert-file-contents (locate-library "qss.el"))
    (if (re-search-forward "^;; Version: \\(.*\\)$" nil t) (match-string 1) "?")))

(defun qss-manual-reference ()
  "Return the reference chapter as LaTeX."
  (concat
   "% Written by qss-manual-reference.el from the code of qss "
   (qss-manual--version)
   ".  Do not edit.\n"
   "\\section{Words}\\label{ref:words}\n" (qss-manual--words)
   "\\section{Sources}\\label{ref:sources}\n" (qss-manual--sources)
   "\\section{Options}\\label{ref:options}\n"
   "All options are in the customization group \\code{qss} (\\code{M-x customize-group RET qss}).\n\n"
   (qss-manual--options)
   "\\section{Faces}\\label{ref:faces}\n" (qss-manual--faces)
   "\\section{Presentation types}\\label{ref:types}\n"
   "\\enquote{Text} says whether typed text can be read as the type.\n\n"
   (qss-manual--presentation-types)
   "\\section{Commands with typed arguments}\\label{ref:commands}\n" (qss-manual--commands)
   "\\section{Named contracts}\\label{ref:contract-types}\n" (qss-manual--contract-types)
   "\\section{Contracts}\\label{ref:contracts}\n"
   (format "The %d functions of the interface with their contracts, as \\file{qss-interface.el} defines them.\n\n"
           (length (qss-contract-functions)))
   (qss-manual--contracts)))

(defun qss-manual--write-example-parts (dir)
  "Write the parts of qss-manual-example.el in DIR as example-N.el.txt.
A part runs from a line \";;; N. ...\" to the next such line or to
\"(provide\"; the marker line itself is left out."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "qss-manual-example.el" dir))
    (goto-char (point-min))
    (while (re-search-forward "^;;; \\([0-9]+\\)\\. .*\n" nil t)
      (let ((n (match-string 1))
            (start (point))
            (end (save-excursion
                   (if (re-search-forward "^\\(;;; [0-9]+\\. \\|(provide \\)" nil t)
                       (match-beginning 0)
                     (point-max)))))
        (let ((text (string-trim (buffer-substring-no-properties start end))))
          ;; .el.txt: not a file of Lisp to load or compile, only a part to show
          (with-temp-file (expand-file-name (format "example-%s.el.txt" n) dir)
            (insert text "\n")))))))

(defun qss-manual-write-reference ()
  "Write reference.tex and the parts of the example next to this file."
  (let* ((dir (file-name-directory (or load-file-name (locate-library "qss-manual-reference"))))
         (file (expand-file-name "reference.tex" dir)))
    (with-temp-file file (insert (qss-manual-reference)))
    (with-temp-file (expand-file-name "version.tex" dir)
      (insert "% Written by qss-manual-reference.el.  Do not edit.\n"
              (format "\\newcommand{\\qssversion}{%s}\n" (qss-manual--version))))
    (qss-manual--write-example-parts dir)
    (message "Wrote %s and the parts of the example" file)))

(defun qss-manual-build ()
  "Write the reference and build qss-manual.pdf with LuaLaTeX (twice)."
  (interactive)
  (let ((default-directory (file-name-directory (locate-library "qss-manual-reference"))))
    (qss-manual-write-reference)
    (dotimes (_ 2)
      (unless (zerop (call-process "lualatex" nil "*qss-manual*" nil
                                   "-interaction=nonstopmode" "qss-manual.tex"))
        (pop-to-buffer "*qss-manual*")
        (user-error "LuaLaTeX failed; see *qss-manual*")))
    (message "Built %s" (expand-file-name "qss-manual.pdf"))))

(provide 'qss-manual-reference)
;;; qss-manual-reference.el ends here
