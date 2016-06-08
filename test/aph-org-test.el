;;; aph-org-test.el --- Tests for aph-org.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-org', `ert'

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
(eval-when-compile (require 'aph-ert))


;;;; Number Twiddling
;;===================
(ert-deftest aph/org-increase-number ()
  "Test `aph/org-increase-number'."
  (aph/ert-with-buffer 'org-mode "
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
