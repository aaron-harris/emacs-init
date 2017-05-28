;;; morgue-test.el --- Tests for morgue.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `morgue', `ert'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'morgue)
(require 'ert)

(eval-when-compile '(require 'cl-lib))


;;; Testing Apparatus
;;;==================
(defmacro morgue-test (s &rest test-pairs)
  "Expand into code running TEST-PAIRS starting from S (a string).

Each TEST-PAIR is a pair (TRANSFORM . RESULT), where TRANSFORM is
a function suitable for use with `morgue-apply' and RESULT is a
string.

The expanded code puts S on a fresh kill ring (one that has been
let-bound for testing purposes), then applies `morgue-apply' with
each TRANSFORM individually and tests that RESULT is returned.

After all tests have been performed, the code checks that the
kill ring contains all results in sequence."
  (declare (indent 1)
           (debug (stringp &rest (function-form . stringp))))
  (let (results)
    `(let (kill-ring)
       (with-temp-buffer
         (kill-new ,s)
         ,@(cl-loop for (transform . result) in test-pairs
                   do (push result results)
                   collect
                   `(should (equal ,result (morgue-apply ,transform))))
         (should (equal kill-ring '(,@results ,s)))))))

(defmacro morgue-test-all (s result &rest transforms)
  "Expand into code testing that TRANSFORMS give RESULT ON S.

Put S (a string) on a fresh kill ring (let-bound for testing
purposes), apply `morgue-apply' to transforms (all together), and
test that RESULT is returned, and that the kill ring is updated
appropriately."
  (declare (indent 2)
           (debug (stringp stringp &rest function-form)))
  `(let (kill-ring)
     (with-temp-buffer
       (kill-new ,s)
       (morgue-apply ,@transforms)
       (should (equal kill-ring '(,result ,s))))))


;;; Applicator Tests
;;;=================
(ert-deftest morgue-test-apply ()
  "Test basic functionality of `morgue-apply'."
  (morgue-test "Foo"
     (#'identity                          . "Foo")
     ((lambda (kill) (concat kill "bar")) . "Foobar"))
  (morgue-test-all "Foo" "FoobarFoobar"
    (lambda (s) (concat s "bar"))
    (lambda (s) (concat s s)))
  (morgue-test-all "Foo" "Foo"))

(ert-deftest morgue-test-yank ()
  "Test `morgue-yank'."
  (let (kill-ring)
    (with-temp-buffer
      (kill-new "Foo")
      (should (equal "FooFoobar"
                     (morgue-yank (lambda (s) (concat s s))
                                  (lambda (s) (concat s "bar")))))
      (should (equal "FooFoobar"
                     (buffer-string)))
      (should (equal '("FooFoobar" "Foo")
                     kill-ring)))))


;;; Transform Tests
;;;================
(ert-deftest morgue-test-map ()
  "Test `morgue-map'."
  (morgue-test "Foo\nbar" 
    ((morgue-map (lambda (s) (concat s s))) . "FooFoo\nbarbar")
    ((morgue-map #'identity "\nbar" " ")    . "FooFoo bar")))

(ert-deftest morgue-test-zap ()
  "Test `morgue-zap'."
  (morgue-test "Foo\nbar\nbaz"
    ((morgue-zap "\n") . "bar\nbaz")
    ((morgue-zap "ba") . "r\nbaz")))

(provide 'morgue-test)
;;; morgue-test.el ends here
