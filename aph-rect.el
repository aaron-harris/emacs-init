;;; aph-rect.el --- Rectangle extensions             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `rect'

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

;; Code extending the `rect' module built into Emacs.

;;; Code:

(require 'rect)

(defun aph/yank-rectangle-from-kill-ring (&optional arg verbose)
  "Yank the top of kill ring as a rectangle.
Make the \"last killed rectangle\" be the top entry of the kill
ring, then yank that rectangle at point.

With \\[universal-argument] as argument, just save the top entry
of the kill ring as a rectangle, without yanking.  Print a
message to that effect.  When called from elisp, this message is
suppressed unless the optional argument VERBOSE is supplied.
 
With argument N, save the Nth most recent kill instead of the
most recent."
  (interactive "P\np")
  (let ((n          (if (numberp arg) arg 1))
        (save-only  (and arg (listp arg)))
        (width      0))
    (with-temp-buffer
      (yank n)
      ;; Scan for maximum line width
      (dotimes (i (point-max))
        (goto-char (1+ i))
        (setq width (max width (current-column))))
      ;; Pad final line to max width
      (while (< (current-column) width)
        (insert " ")) 
      (copy-rectangle-as-kill 1 (point-max)))
    (if save-only
        (when verbose (message "Most recent kill saved as rectangle."))
      (yank-rectangle))))

(provide 'aph-rect)
;; aph-rect.el ends here
