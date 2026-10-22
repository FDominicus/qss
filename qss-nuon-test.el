;;; qss-nuon-test.el --- Tests for the NUON reader and writer  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Friedrich Dominicus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Reading and writing NUON needs no Nushell; the round trip through
;; Nushell is skipped when `qss-nu-program' is not installed.
;;
;;   emacs --batch -L . -l qss-nuon-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'qss)

(defun qss-nuon-test--time (text)
  "Return TEXT as a Lisp time."
  (encode-time (iso8601-parse text)))

(ert-deftest qss-nuon-read-scalars ()
  (should (equal (qss-nuon-read "42") 42))
  (should (equal (qss-nuon-read "-12") -12))
  (should (equal (qss-nuon-read "1.5") 1.5))
  (should (equal (qss-nuon-read "100000000000000000000.0") 1e20))
  (should (equal (qss-nuon-read "0.0000001") 1e-7))
  (should (isnan (qss-nuon-read "NaN")))
  (should (equal (qss-nuon-read "inf") 1.0e+INF))
  (should (equal (qss-nuon-read "-inf") -1.0e+INF))
  (should (equal (qss-nuon-read "9223372036854775807") 9223372036854775807))
  (should (eq (qss-nuon-read "true") t))
  (should (eq (qss-nuon-read "false") nil))
  (should (eq (qss-nuon-read "null") :null))
  (should (eq (qss-nuon-read "") :null))
  (should (eq (qss-nuon-read "  \n") :null)))

(ert-deftest qss-nuon-read-strings ()
  (should (equal (qss-nuon-read "foo") "foo"))
  (should (equal (qss-nuon-read "/tmp/cc-socks") "/tmp/cc-socks"))
  (should (equal (qss-nuon-read "back\\slash") "back\\slash"))
  (should (equal (qss-nuon-read "\"a b\\\"c\"") "a b\"c"))
  (should (equal (qss-nuon-read "\"x\\ny\\t\\\\\"") "x\ny\t\\"))
  (should (equal (qss-nuon-read "\"line\nbreak\"") "line\nbreak"))
  (should (equal (qss-nuon-read "\"\\u{1F600}\\u00e9\"") "\U0001F600é"))
  (should (equal (qss-nuon-read "é") "é"))
  (should (equal (qss-nuon-read "'single \"x\"'") "single \"x\""))
  (should (equal (qss-nuon-read "`z w`") "z w"))
  (should (equal (qss-nuon-read "r#'x\"y'#") "x\"y"))
  (should (equal (qss-nuon-read "r##'a'#b'##") "a'#b"))
  ;; quoted words that look like other things stay strings
  (should (equal (qss-nuon-read "[\"true\", \"null\", \"1\", \"10kb\"]")
                 '("true" "null" "1" "10kb"))))

(ert-deftest qss-nuon-read-types ()
  (should (equal (qss-nuon-read "10000b") (qss-nuon-size 10000)))
  (should (equal (qss-nuon-read "10kb") (qss-nuon-size 10000)))
  (should (equal (qss-nuon-read "1.5KiB") (qss-nuon-size 1536)))
  (should (equal (qss-nuon-read "-1000b") (qss-nuon-size -1000)))
  (should (equal (qss-nuon-read "10800000000000ns") (qss-nuon-duration 10800000000000)))
  (should (equal (qss-nuon-read "3hr") (qss-nuon-duration 10800000000000)))
  (should (equal (qss-nuon-read "2µs") (qss-nuon-duration 2000)))
  (should (equal (qss-nuon-read "-5sec") (qss-nuon-duration -5000000000)))
  (should (time-equal-p (qss-nuon-read "2026-10-22T14:40:00+02:00")
                        (qss-nuon-test--time "2026-10-22T14:40:00+02:00")))
  ;; the nanoseconds stay
  (should (equal (qss-nuon-read "2026-10-22T14:40:36.744546788+02:00")
                 (cons (+ (* (time-convert (qss-nuon-test--time "2026-10-22T14:40:36+02:00") 'integer)
                             1000000000)
                          744546788)
                       1000000000)))
  (should (time-equal-p (qss-nuon-read "2026-10-22") (qss-nuon-test--time "2026-10-22T00:00:00Z")))
  (should (equal (qss-nuon-read "[1..5, 1..<5, 1..3..10, 1..]") '("1..5" "1..<5" "1..3..10" "1..")))
  (should (equal (qss-nuon-read "0x[01FF]") (unibyte-string 1 255)))
  (should (equal (qss-nuon-read "0x[]") "")))

(ert-deftest qss-nuon-read-structures ()
  (should (equal (qss-nuon-read "[1, 2, 3]") '(1 2 3)))
  (should (equal (qss-nuon-read "[1 2 3]") '(1 2 3)))
  (should (equal (qss-nuon-read "[]") nil))
  (should (equal (qss-nuon-read "{a: 1, \"b c\": [], c: {x: [1, 2]}}")
                 '((a . 1) (b\ c) (c (x 1 2)))))
  (should (equal (qss-nuon-read "{}") nil))
  (should (equal (qss-nuon-read "[{a-b: 1, \"1x\": 2}]") '(((a-b . 1) (1x . 2)))))
  ;; the table syntax
  (should (equal (qss-nuon-read "[[name, size]; [foo, 10kb], [\"with space\", 1b]]")
                 (list (list '(name . "foo") (cons 'size (qss-nuon-size 10000)))
                       (list '(name . "with space") (cons 'size (qss-nuon-size 1))))))
  (should (equal (qss-nuon-read "[[a]; [[[x]; [1]]]]") '(((a ((x . 1)))))))
  (should (equal (qss-nuon-read "[[a, b]; ]") nil))
  ;; comments and missing commas
  (should (equal (qss-nuon-read "# head\n[1, # one\n 2\n 3]") '(1 2 3))))

(ert-deftest qss-nuon-read-errors ()
  (should-error (qss-nuon-read "[1, 2") :type 'qss-nuon-error)
  (should-error (qss-nuon-read "{a 1}") :type 'qss-nuon-error)
  (should-error (qss-nuon-read "\"open") :type 'qss-nuon-error)
  (should-error (qss-nuon-read "1 2") :type 'qss-nuon-error))

(ert-deftest qss-nuon-plain ()
  (should (equal (qss-nuon-plain (list :a (qss-nuon-size 5) :b (list (qss-nuon-duration 7) 1)))
                 '(:a 5 :b (7 1))))
  (should (equal (qss-nuon-plain (cons 1 (qss-nuon-size 2))) '(1 . 2)))
  ;; long lists do not recurse deeply
  (should (= (length (qss-nuon-plain (make-list 100000 (qss-nuon-size 1)))) 100000)))

(ert-deftest qss-nuon-encode ()
  (should (equal (qss-nuon-encode nil) "null"))
  (should (equal (qss-nuon-encode t) "true"))
  (should (equal (qss-nuon-encode 42) "42"))
  (should (equal (qss-nuon-encode 1.5) "1.5"))
  (should (equal (qss-nuon-encode 0.0e+NaN) "NaN"))
  (should (equal (qss-nuon-encode "x\"\n\1") "\"x\\\"\\n\\u{1}\""))
  (should (equal (qss-nuon-encode 'sym) "\"sym\""))
  (should (equal (qss-nuon-encode '(1 2)) "[1, 2]"))
  (should (equal (qss-nuon-encode '(:a 1 :b (:c nil))) "{\"a\": 1, \"b\": {\"c\": null}}"))
  (should (equal (qss-nuon-encode 10 'size) "10b"))
  (should (equal (qss-nuon-encode 5 'duration) "5ns"))
  (should (equal (qss-nuon-encode (qss-nuon-size 3)) "3b"))
  (should (string-match-p "\\`2026-10-22T10:00:00\\.000000000[+-][0-9][0-9]:[0-9][0-9]\\'"
                          (qss-nuon-encode (qss-nuon-test--time "2026-10-22T10:00:00+02:00") 'time)))
  ;; a time with a fraction cannot be a list: it is written as a time
  (let ((time (qss-nuon-read "2026-10-22T08:00:00.5Z")))
    (should (equal (qss-nuon-encode time) (format-time-string "%FT%T.%N%:z" time)))
    (should (string-match-p "\\.500000000" (qss-nuon-encode time))))
  ;; without the type a time is a list of numbers
  (should (string-prefix-p "[" (qss-nuon-encode (qss-nuon-test--time "2026-10-22T10:00:00Z")))))

(ert-deftest qss-nuon-encode-table ()
  (let ((tab (qss-make-table "t" (list (list :n "a" :size 10 :d 5 :x '(1 2)))
                             '(n (size :type size) (d :type duration) x))))
    (should (equal (qss-nuon-encode-table tab)
                   "[{\"n\": \"a\", \"size\": 10b, \"d\": 5ns, \"x\": [1, 2]}]")))
  (should (equal (qss-nuon-encode-table (qss-as-table (qss-> '((:n 1) (:n 2)) (get n)))) "[1, 2]"))
  (should (equal (qss-nuon-encode-table (qss-make-table "t" nil '(a))) "[]")))

(ert-deftest qss-nuon-read-what-it-writes ()
  (let ((v '(:a 1 :b "x \"y\"\n" :c (1 2.5) :d nil)))
    (should (equal (qss-nuon-read (qss-nuon-encode v))
                   '((a . 1) (b . "x \"y\"\n") (c 1 2.5) (d . :null))))))

(ert-deftest qss-nuon-durations-and-quantities ()
  (should (equal (qss-format-duration 0) "0sec"))
  (should (equal (qss-format-duration 500) "500ns"))
  (should (equal (qss-format-duration 1500) "1µs 500ns"))
  (should (equal (qss-format-duration 1500000000) "1sec 500ms"))
  (should (equal (qss-format-duration 12000000000000) "3hr 20min"))
  (should (equal (qss-format-duration 10800000000000) "3hr"))
  (should (equal (qss-format-duration -5000000000) "-5sec"))
  (should (equal (qss-format-duration 90061000000000) "1day 1hr"))
  (should (equal (qss-parse-quantity "10kb") '(size . 10000)))
  (should (equal (qss-parse-quantity "1.5KiB") '(size . 1536)))
  (should (equal (qss-parse-quantity "3hr") '(duration . 10800000000000)))
  (should (null (qss-parse-quantity "10")))
  (should (null (qss-parse-quantity "10xyz")))
  ;; comparisons with sizes and durations as text
  (let ((rows '((:n "a" :size 5000 :d 1000000000) (:n "b" :size 50000 :d 7200000000000))))
    (should (equal (qss-> rows (where (> :size "10kb")) (get n) collect) '("b")))
    (should (equal (qss-> rows (where (< :d "1hr")) (get n) collect) '("a")))
    (should (equal (qss-> rows (where (= :size "5kb")) (get n) collect) '("a")))))

;;; With Nushell

(defmacro qss-nuon-test--with-nu (&rest body)
  "Run BODY if Nushell is installed, else skip."
  (declare (indent 0))
  `(progn (skip-unless (executable-find qss-nu-program))
          (let ((qss-nu-format 'nuon)) ,@body)))

(ert-deftest qss-nuon-types-from-nushell ()
  (qss-nuon-test--with-nu
    (let ((tab (qss-nu "[{name: a, size: 10kb, took: 3hr, at: 2026-10-22T10:00:00+02:00, n: 1}
                         {name: b, size: 2mb, took: 90sec, at: 2026-10-23T10:00:00+02:00, n: 2}]")))
      (should (equal (mapcar (lambda (s) (cons (car s) (plist-get (cdr s) :type)))
                             (qss-table-columns tab))
                     '((name) (size . size) (took . duration) (at . time) (n . number))))
      (should (equal (qss-> tab (get size) collect) '(10000 2000000)))
      (should (equal (qss-> tab (get took) collect) '(10800000000000 90000000000)))
      (should (equal (qss-> tab (where (> :took "1hr")) (get name) collect) '("a"))))
    ;; sizes that no column name gives away, and single values
    (should (equal (qss-collect (qss-nu "[1kb 2kb]")) '(1000 2000)))
    (should (equal (plist-get (cdr (assq 'value (qss-table-columns (qss-nu "[1kb 2kb]")))) :type)
                   'size))
    (should (equal (qss-nu "1kb + 1kb") 2000))
    (should (equal (qss-nu "3hr") 10800000000000))
    ;; in a single record the value column shows the types readably
    (should (equal (qss-collect (qss-nu "{s: 2048b, d: 90min}"))
                   '((:column s :value "2k") (:column d :value "1hr 30min"))))))

(ert-deftest qss-nuon-round-trip-through-nushell ()
  "Tables go to Nushell and back with all their types."
  (qss-nuon-test--with-nu
    (let* ((time (qss-nuon-test--time "2026-10-22T10:00:00+02:00"))
           (tab (qss-make-table "t" (list (list :name "a \"q\"\n" :size 2048 :took 5000000000
                                                :at time :ok t :none nil :nested '(:x (1 2)))
                                          (list :name "b" :size 10 :took 7 :at time :ok nil
                                                :none nil :nested nil))
                                '(name (size :type size) (took :type duration) (at :type time)
                                  ok none nested))))
      (should (equal (qss-> tab (nu "get size | math sum")) 2058))
      (should (equal (qss-> tab (nu "where took > 1sec | get name") collect) '("a \"q\"\n")))
      (should (equal (qss-> tab (nu "first | get size | describe")) "filesize"))
      (should (equal (qss-> tab (nu "first | get took | describe")) "duration"))
      (should (equal (qss-> tab (nu "first | get at | describe")) "datetime"))
      (let ((back (qss-> tab (nu "$in"))))
        (should (equal (qss-collect back)
                       (list (list :name "a \"q\"\n" :size 2048 :took 5000000000 :at time :ok t
                                   :none nil :nested '(:x (1 2)))
                             (list :name "b" :size 10 :took 7 :at time :ok nil
                                   :none nil :nested nil))))
        (should (equal (mapcar (lambda (s) (plist-get (cdr s) :type)) (qss-table-columns back))
                       '(nil size duration time nil nil nil)))))))

(ert-deftest qss-nuon-ls-sizes ()
  (qss-nuon-test--with-nu
    (let ((dir (make-temp-file "qss-nuon-ls-" t)))
      (unwind-protect
          (let ((default-directory (file-name-as-directory dir)))
            (with-temp-file "f.txt" (insert (make-string 3000 ?x)))
            (let ((tab (qss-nu "ls")))
              (should (equal (qss-> tab (get size) collect) '(3000)))
              (should (eq (plist-get (cdr (assq 'size (qss-table-columns tab))) :type) 'size))
              (should (eq (plist-get (cdr (assq 'modified (qss-table-columns tab))) :type) 'time))))
        (delete-directory dir t)))))

(provide 'qss-nuon-test)
;;; qss-nuon-test.el ends here
