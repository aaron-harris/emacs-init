;;; morgue-test.el --- Tests for morgue.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

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

;;; Commentary:

;; 

;;; Code:

(require 'morgue)
(require 'aph-ert)

(eval-when-compile '(require 'cl-lib))


;;; Testing Apparatus
;;;==================
(defmacro morgue-test (s &rest test-pairs)
  "Expand into code running TEST-PAIRS starting from S (a string).

Each TEST-PAIR is a pair (TRANSFORM . RESULT), where TRANSFORM is
a function suitable for use with `morgue-transform' and RESULT
is a string.

The expanded code puts S on a fresh kill ring (one that has been
let-bound for testing purposes), then applies `morgue-transform'
with each TRANSFORM and tests that RESULT is returned.

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
                   `(should (equal ,result (morgue-transform ,transform))))
         (should (equal kill-ring '(,@results ,s)))))))


;;; Basic Tests
;;;============
(ert-deftest morgue-test-transform ()
  "Test basic functionality of `morgue-transform'."
  (morgue-test "Foo"
     (#'identity                          . "Foo")
     ((lambda (kill) (concat kill "bar")) . "Foobar")))


;;; Transform Tests
;;;================
(ert-deftest morgue-map-join ()
  "Test `morgue-map'."
  (morgue-test "Foo bar"
    ((morgue-map nil "X")                           . "FooXbar")
    ((morgue-map "X" " " (lambda (s) (concat s s))) . "FooFoo barbar")))

(provide 'morgue-test)
;;; morgue-test.el ends here
