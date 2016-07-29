;;; aph-org-test.el --- Tests for aph-org.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-org', `proctor'

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

(require 'aph-org)
(require 'proctor)


;;;; Narrowing
;;============
(ert-deftest aph/org-test-narrow-to-entry ()
  "Test `aph/org-narrow-to-entry'."
  (proctor-with-buffer 'org-mode "
* Foo
  Foo entry text
** Bar
   Bar entry text"
    (aph/org-narrow-to-entry)
    (should (equal (buffer-string)
                   "* Foo\n  Foo entry text\n"))))


;;;; Timestamps
;;=============
(ert-deftest aph/org-test-timestamp-date-only ()
  "Test `aph/org-timestamp-date-only'."
  (proctor-test-all #'aph/org-timestamp-date-only #'equal
    (("<2016-07-22 Fri 16:00>") . "<2016-07-22 Fri>")
    (("[2016-07-22 Fri]")       . "[2016-07-22 Fri]")))

(ert-deftest aph/org-test-find-timestamp ()
  "Test `aph/org-find-timestamp'."
  (proctor-with-buffer 'org-mode "
* TODO Foo
  SCHEDULED: <2016-07-22 Fri>
  DEADLINE: <2016-07-23 Sat>
  CLOSED: [2016-07-25 Mon]
  <2016-07-24 Sun 06:00> [2016-07-23 Sat]
  <2016-07-22 Fri>" 
    ;; Success cases
    (proctor-test-all
        #'aph/org-find-timestamp
        (lambda (return expected)
          (should (numberp return))
          (should (= return (point)))
          (should (looking-back expected)))
      (()                              . "<2016-07-22 Fri>")
      ((nil all)                       . "<2016-07-22 Fri>")
      ((nil active)                    . "<2016-07-22 Fri>")
      ((nil inactive)                  . "[[]2016-07-25 Mon[]]")
      ((nil scheduled)                 . "<2016-07-22 Fri>")
      ((nil deadline)                  . "<2016-07-23 Sat>")
      ((nil active "<2016-07-24 Sun>") . "<2016-07-24 Sun 06:00>"))
    ;; Failure cases
    (goto-char (point-min))
    (should-not (aph/org-get-entry-timestamp-marker
                 nil nil "<2016-07-24 Sun>"))
    (should (= (point) (point-min)))))

(ert-deftest aph/org-test-find-timestamp:nested ()
  "Test `aph/org-find-timestamp' on nested items.

This is a regression test for a bug wherein
`aph/org-find-timestamp' could find a timestamp in a sub-item."
  (proctor-with-buffer 'org-mode "
* TODO Foo
** TODO Bar
   <2016-07-29 Fri>"
    (should-not (aph/org-find-timestamp (point-min) 'active))))

(ert-deftest aph/org-test-relative-timestamp:days ()
  "Test `aph/org-relative-timestamp' for differences of days."
  (proctor-test-all
      (lambda (&rest args)
        (org-time-stamp-to-now (apply #'aph/org-relative-timestamp args)))
      #'=
    (nil    . 0)
    ((0)    . 0)
    ((0 0)  . 0)
    ((0 24) . 1)
    ((1)    . 1)
    ((-1)   . -1)
    ((5)    . 5)))

(ert-deftest aph/org-test-relative-timestamp:hours ()
  "Test `aph/org-relative-timestamp' for differences of hours."
  (proctor-test-all
      (lambda (&rest args)
        (org-time-stamp-to-now (apply #'aph/org-relative-timestamp args)
                               :seconds))
      (lambda (x y)
        (<= (- x 60) y (+ x 60)))
    ((nil 1) . ,(* 1  60 60))
    ((1   3) . ,(* 27 60 60))))

(ert-deftest aph/org-test-relative-timestamp:specificity ()
  "Test that `aph/org-relative-timestamp' omits hours correctly."
  (let ((regexp "[0-9][0-9]:[0-9][0-9]>$"))
    (should-not (string-match-p regexp (aph/org-relative-timestamp)))
    (should     (string-match-p regexp (aph/org-relative-timestamp nil 0)))))

(ert-deftest aph/org-test-relative-timestamp:inactive ()
  "Test `aph/org-relative-timestamp' for inactive timestamps."
  (proctor-test-all
      (lambda (type &rest args) 
        (string-match-p (org-re-timestamp type)
                        (apply #'aph/org-relative-timestamp args)))
      (lambda (a b)
        (not (org-xor a b)))
    ((active   nil nil nil) . 0)
    ((active   0   0   nil) . 0)
    ((inactive nil nil t)   . 0)
    ((inactive 3   5   t)   . 0)))


;;;; Number Twiddling
;;===================
(ert-deftest aph/org-increase-number ()
  "Test `aph/org-increase-number'." 
  (proctor-with-buffer 'org-mode "
| A  | B   |
|----+-----|
|  1 | foo |
|    |  15 |"
    (forward-line 2)
    (forward-char 2)
    (should (looking-at-p " 1 |"))
    (aph/org-increase-number 5)
    (org-table-beginning-of-field 1)
    (should (looking-at-p "6 |"))
    (forward-line 1) 
    (should (looking-at-p "| +| +15"))
    (should-error (aph/org-increase-number))
    (should (looking-at-p " +| +15"))))

(provide 'aph-org-test)
;;; aph-org-test.el ends here
