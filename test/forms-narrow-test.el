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
(require 'proctor-forms)


;;;; Navigation Commands
;;======================
(ert-deftest forms-narrow-test-next/prev-record ()
  "Test `forms-narrow-next-record', `forms-narrow-prev-record'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "") ("3" "bar") ("4" "")) 
    (forms-narrow
     (lambda () (not (equal "" (nth 2 forms-fields)))))
    (forms-first-record)
    (forms-narrow-next-record 1)
    (should (= forms--current-record 3))
    (should-error (forms-narrow-next-record 1))
    (should (= forms--current-record 3))
    (should-error (forms-narrow-prev-record 5))
    (should (= forms--current-record 1))))

(ert-deftest forms-narrow-test-first/last-record ()
  "Test `forms-narrow-first-record', `forms-narrow-last-record'."
  (proctor-forms-with-db nil
      (("1" "") ("2" "foo") ("3" "bar") ("4" ""))
    (forms-narrow
     (lambda () (not (equal "" (nth 2 forms-fields)))))
    (forms-narrow-first-record)
    (should (= forms--current-record 2))
    (forms-narrow-last-record)
    (should (= forms--current-record 3))
    (forms-narrow
     (lambda () (equal "" (nth 2 forms-fields))))
    (forms-narrow-first-record)
    (should (= forms--current-record 1))
    (forms-narrow-last-record)
    (should (= forms--current-record 4))))

(ert-deftest forms-narrow-test-nil-predicate ()
  "Test `forms-narrow' functions with `forms-narrow--predicate' nil."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" ""))
    (should-not forms-narrow-mode)
    (should (null forms-narrow--predicate))
    (forms-first-record)
    (forms-narrow-next-record 1)
    (should (= forms--current-record 2))))


;;;; Entry and Exit Points
;;========================
(ert-deftest forms-narrow-test-narrowing ()
  "Test narrowing functions for `forms-narrow-mode'.
The commands tested are `forms-narrow', `forms-narrow-widen',
`forms-narrow-again'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" ""))
    (should-error forms-narrow-again)
    (should-not forms-narrow-mode)
    (let ((pred (lambda () (not (equal "" (nth 2 forms-fields))))))
      (forms-narrow pred)
      (should forms-narrow-mode)
      (should (equal forms-narrow--predicate pred))
      (forms-narrow-widen)
      (should-not forms-narrow-mode)
      (forms-narrow-again)
      (should forms-narrow-mode)
      (should (equal forms-narrow--predicate pred)))))

(ert-deftest forms-narrow-test-rebase ()
  "Test `forms-narrow--rebase'."
  (proctor-forms-with-db nil
      (("1") ("2") ("3") ("4") ("5"))
    (proctor-test-all
        (lambda (rec mode)
          (forms-narrow-widen)
          (forms-jump-record rec)
          (let ((forms-narrow-rebase-mode mode))
            (forms-narrow-list '(2 4))
            forms--current-record))
        #'=
      ((3 :first) . 2) 
      ((3 :next)  . 4)
      ((2 :next)  . 2)
      ((5 :next)  . 2)
      ((3 nil)    . 3))))

(ert-deftest forms-narrow-test-rebase:widen ()
  "Test `forms-narrow--rebase' with `forms-narrow-rebase-widen'."
  (proctor-forms-with-db nil
      (("1") ("2") ("3"))
    (let ((forms-narrow-rebase-widen t)
          (forms-narrow-rebase-mode  :first)) 
      (forms-narrow-list '(2))
      (should (= 2 forms--current-record))
      (forms-narrow-widen)
      (should (= 1 forms--current-record)))))


;;;; Narrowing Commands and Subroutines
;;=====================================
(ert-deftest forms-narrow-test-regexp ()
  "Test `forms-narrow-regexp'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "bar") ("foobar" "3"))
    (forms-narrow-regexp "foo")
    (forms-first-record)
    (forms-narrow-next-record 1)
    (should (= forms--current-record 3))
    (forms-narrow-prev-record 1)
    (should (= forms--current-record 1))))

(ert-deftest forms-narrow-test-list ()
  (proctor-forms-with-db nil
      (("1") ("2") ("3") ("4"))
    (forms-narrow-list '(2 4))
    (forms-narrow-first-record)
    (should (= forms--current-record 2))
    (forms-narrow-next-record 1)
    (should (= forms--current-record 4))
    (should-error (forms-narrow-next-record 1))))

(provide 'forms-narrow-test)
;;; forms-narrow-test.el ends here
