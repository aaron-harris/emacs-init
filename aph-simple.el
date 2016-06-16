;;; aph-simple.el --- Extensions for `simple' module -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `simple'

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

;; Extensions for the `simple' module built into Emacs.

;;; Code:

(require 'simple)


;;;; Newline Commands
;;===================
(defun aph/open-line (n)
  "As `open-line', with support for negative argument.
An argument of -N calls `join-line' with an argument N times."
  (interactive "p")
  (if (< n 0)
      (dotimes (i (- n)) (join-line :invert))
    (open-line n)))


;;;; Motion Commands
;;==================
(defun aph/move-beginning-of-line (&optional arg)
  "Combine `move-beginning-of-line' and `back-to-indentation'.

Behave as `move-beginning-of-line', unless point is already at
beginning of line, in which case call `back-to-indentation'.

If ARG is supplied, then it is interpreted as in
`move-beginning-of-line' and `back-to-indentation' is not
called.

Return the new value of point."
  (interactive "^P")
  (cond
   (arg     (move-beginning-of-line (prefix-numeric-value arg)))
   ((bolp)  (back-to-indentation))
   (:else   (move-beginning-of-line 1)))
  (point))

(provide 'aph-simple)
;;; aph-simple.el ends here
