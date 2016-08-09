;;; forms-skip-test.el --- Tests for forms-skip.el   -*- lexical-binding: t; -*-

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

(require 'forms-skip)
(require 'proctor)


;;;; Navigation Commands
;;======================
(ert-deftest forms-skip-test-forward/backward ()
  "Test `forms-skip-forward', `forms-skip-backward'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "") ("3" "bar") ("4" ""))
    (setq forms-skip-predicate
          (lambda () (not (equal "" (nth 2 forms-fields)))))
    (forms-first-record) 
    (forms-skip-forward 1)
    (should (= forms--current-record 3)) 
    (should-error (forms-skip-forward 1))
    (should (= forms--current-record 3))
    (should-error (forms-skip-backward 5))
    (should (= forms--current-record 1))))

(ert-deftest forms-skip-test-nil-predicate ()
  "Test `forms-skip' functions with `forms-skip-predicate' nil."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" ""))
    (should (null forms-skip-predicate))
    (forms-first-record)
    (forms-skip-forward 1)
    (should (= forms--current-record 2))))

(provide 'forms-skip-test)
;;; forms-skip-test.el ends here
