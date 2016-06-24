;;; aph-org-table-test.el --- Tests for aph-org-table.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-org-table', `proctor'

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

(require 'aph-org-table)
(require 'proctor)


;;;; Subroutines
;;==============
(ert-deftest aph/org-table-test-end-of-this-field ()
  "Test `aph/org-table-end-of-this-field'."
  (proctor-with-buffer 'org-mode "
| A | B   |
|---+-----|
| 1 | foo |"
    (forward-char 2)
    (should (looking-at-p "A"))
    (aph/org-table-end-of-this-field)
    (should (looking-at-p " | B"))
    (aph/org-table-end-of-this-field)
    (should (looking-at-p " | B"))))


;;;; Editing Functions
;;====================
(ert-deftest aph/org-table-test-clear-row-forward ()
  "Test `aph/org-table-clear-row-forward'."
  (proctor-with-buffer 'org-mode "
| A | B     | C     |
|---+-------+-------|
| 1 | foo   | bar   |
| 2 | alpha | bravo |"
    (forward-line 2)
    (forward-char 3)
    (should (looking-at-p " | foo"))
    (aph/org-table-clear-row-forward)
    (should (looking-at-p " |       |       |"))))

(provide 'aph-org-table-test)
;;; aph-org-table-test.el ends here
