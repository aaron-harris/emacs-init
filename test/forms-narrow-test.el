;;; forms-narrow-test.el --- Tests for forms-narrow.el  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'forms-narrow)
(require 'proctor)


;;;; Navigation Commands
;;======================
(ert-deftest forms-narrow-test-next/prev-record ()
  "Test `forms-narrow-next-record', `forms-narrow-prev-record'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "") ("3" "bar") ("4" ""))
    (setq forms-narrow-predicate
          (lambda () (not (equal "" (nth 2 forms-fields)))))
    (forms-first-record)
    (forms-narrow-next-record 1) 
    (should (= forms--current-record 3))
    (should-error (forms-narrow-next-record 1))
    (should (= forms--current-record 3))
    (should-error (forms-narrow-prev-record 5))
    (should (= forms--current-record 1))))

(ert-deftest forms-narrow-test-nil-predicate ()
  "Test `forms-narrow' functions with `forms-narrow-predicate' nil."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" ""))
    (should (null forms-narrow-predicate))
    (forms-first-record)
    (forms-narrow-next-record 1)
    (should (= forms--current-record 2))))

(provide 'forms-narrow-test)
;;; forms-narrow-test.el ends here
