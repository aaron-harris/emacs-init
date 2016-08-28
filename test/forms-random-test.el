;;; forms-random-test.el --- Tests for forms-random.el  -*- lexical-binding: t; -*-

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

(require 'forms-random)
(require 'proctor-forms)


;;;; Basic Randomness
;;===================
(ert-deftest forms-random-test-unweighted ()
  "Test `forms-random-record'."
  (proctor-forms-with-db nil
      (("1") ("2") ("3") ("4"))
    (proctor-random 1000 50
        ((1 . 250) (2 . 250) (3 . 250) (4 . 250))
      (forms-random-record)
      forms--current-record)))

(ert-deftest forms-random-test-unweighted:narrow ()
  "Test `forms-random-record' with `forms-narrow'."
  (proctor-forms-with-db nil
      (("1" "x") ("2" "") ("3" "x") ("4" "x") ("5" "x"))
    (forms-narrow
     (lambda () (equal (nth 2 forms-fields) "x")))
    (proctor-random 1000 50
        ((1 . 250) (3 . 250) (4 . 250) (5 . 250))
      (forms-random-record)
      forms--current-record)))

(ert-deftest forms-random-test-weighted ()
  "Test `forms-random-record-weighted'."
  (proctor-forms-with-db
      (setq forms-random-weight-field 2)
      (("1" "1") ("2" "1") ("3" "2"))
    (proctor-random 1000 50
        ((1 . 250) (2 . 250) (3 . 500))
      (forms-random-record-weighted)
      forms--current-record)))

(ert-deftest forms-random-test-weighted:narrow ()
  "Test `forms-random-record-weighted' with `forms-narrow'."
  (proctor-forms-with-db
      (setq forms-random-weight-field 2)
      (("1" "1" "x") ("2" "5" "") ("3" "2" "x") ("4" "1" "x"))
    (forms-narrow
     (lambda () (equal (nth 3 forms-fields) "x")))
    (proctor-random 1000 50
        ((1 . 250) (3 . 500) (4 . 250))
      (forms-random-record-weighted)
      forms--current-record)))

(provide 'forms-random-test)
;;; forms-random-test.el ends here
