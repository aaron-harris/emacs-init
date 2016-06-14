;;; org-agenda-skip.el --- Extra skip functions      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: calendar

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

;; This module contains some simple extensions to the function
;; `org-agenda-skip-if'.

;;; Code:

(require 'org-agenda)


;;;; Skip Unless
;;==============
(defun org-agenda-skip-unless (subtree conditions)
  "Complement of `org-agenda-skip-if'.

As `org-agenda-skip-if', except the entry or subtree is skipped
in the case where none of the CONDITIONS is true."
  (save-excursion
    (if (org-agenda-skip-if subtree conditions) nil
      (or (outline-next-heading) (point-max)))))

(defun org-agenda-skip-entry-unless (&rest conditions)
  "Skip entry unless any of CONDITIONS is true."
  (org-agenda-skip-unless nil conditions))

(defun org-agenda-skip-subtree-unless (&rest conditions)
  "Skip entry unless any of CONDITIONS is true."
  (org-agenda-skip-unless t conditions))

(provide 'org-agenda-skip)
;;; org-agenda-skip.el ends here
