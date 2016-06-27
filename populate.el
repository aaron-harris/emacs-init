;;; populate.el --- Populate blank lines             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `s', `trinket'

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

;; This module contains commands that look at the region and populate
;; the blank lines inside it using various criteria.  At present, the
;; only such command is `populate-downwards-in-region', which fills
;; each blank line with a copy of the last non-blank line preceding
;; it.

;;; Code:

(require 's)
(require 'trinket)

;;;###autoload
(defun populate-downwards-in-region (beg end)
  "Copy non-blank lines downward in region.

Replace each blank line between BEG and END with the last
non-blank line appearing above it.  Leading blank lines are left
untouched.

Interactively, operate on the region if the region is active.  If
the region begins or ends mid-line, consider it to include that
entire line.  If there is no active region, operate on the entire
buffer.

A blank line for the purposes of this command is a line that
contains only whitespace characters.  It is not necessary that
the line be completely empty."
  (interactive "r")
  (let ((beg  (if (use-region-p) (trinket-bol beg) (point-min)))
	(end  (if (use-region-p) (trinket-eol end) (point-max)))
	stored-line)
    (save-excursion
      (save-restriction
	(goto-char beg)
	(narrow-to-region beg end)
	(while (progn
		 (beginning-of-line)
		 (if (looking-at "^\\s-*?$")
		     (when stored-line (replace-match stored-line))
		   (setq stored-line (s-chomp (thing-at-point 'line))))
		 (end-of-line)
		 (= (forward-line) 0)))))))

(provide 'populate)
;;; populate.el ends here
