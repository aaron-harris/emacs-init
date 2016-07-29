;;; aph-org-agenda-test.el --- Tests for aph-org-agenda.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-org-agenda', `proctor-org'

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

(require 'aph-org-agenda)
(require 'proctor-org)


;;;; Timestamps
;;=============
(ert-deftest aph/org-agenda-test-date-later ()
  "Test `aph/org-agenda-date-later'."
  (proctor-org-with-agenda-items
      ("* TODO Foo\n  %t"
       "* TODO Bar\n  No timestamp")
    (org-todo-list)
    (search-forward "TODO Foo")
    (proctor-test-all
        #'aph/org-agenda-date-later
        (lambda (_ expected)
          (should (equal (org-agenda-get-some-entry-text
                          (org-agenda-get-any-marker) 5)
                         expected)))
      ((1)  . ,(aph/org-relative-timestamp 1))
      ((1)  . ,(aph/org-relative-timestamp 2))
      ((-2) . ,(aph/org-relative-timestamp)))
    (search-forward "TODO Bar")
    (should-error (aph/org-agenda-date-later 1))))

(provide 'aph-org-agenda-test)
;;; aph-org-agenda-test.el ends here
