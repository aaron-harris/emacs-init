;;; org-spin.el --- Select Org headlines randomly    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience, Org

;; Dependencies: `org-child', `validate' (optional)

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

;; This module provides commands that allow you to select an Org
;; heading randomly.  The commands provided (all auto-loaded) are as
;; follows:
;;
;; `org-spin':
;;
;;     Select a child of the heading at point uniformly at random.
;;
;; `org-spin-weighted':
;;
;;     Select a child of the heading at point, using each heading's
;;     value of the property named by the variable
;;     `org-spin-weight-property' as a weighting factor.

;;; Code:

(require 'org-child)


;;;; User Options
;;===============
(defgroup org-spin nil
  "Select headlines randomly."
  :prefix "org-spin-"
  :link '(emacs-commentary-link "org-spin")
  :group 'org)

(defcustom org-spin-weight-property
  "Weight"
  "Name of the property that `org-spin-weighted' should use."
  :type 'string)


;;;; Spinner Commands
;;===================
;;;###autoload
(defun org-spin ()
  "Move point to a random child of heading at point.
Return point.

If point is before the first heading of the buffer, move point to
a random top-level heading."
  (interactive)
  (let ((die-size (org-child-count)))
    (if (zerop die-size) (error "No children")
      (org-child-goto (1+ (random die-size))))))

;;;###autoload
(defun org-spin-weighted (&optional weight-prop)
  "As `org-spin', weighted by property WEIGHT-PROP.

The parameter WEIGHT-PROP should be the name of a property.
Non-negative numeric values for that property are treated as
weights for the spin.  Non-numeric and negative values are
treated as zero.

If all weights are zero, then weights are ignored and the
selection is uniform, as in `org-spin'.

When called interactively or if WEIGHT-PROP is omitted, the value
of `org-spin-weight-property' is used."
  (interactive)
  (when (and (not weight-prop)
	     (require 'validate nil :noerror))
    (validate-variable 'org-spin-weight-property))
  (let* ((weight-prop  (or weight-prop org-spin-weight-property))
	 (die-size     (org-child-sum-property (point) weight-prop))
	 (roll         (if (zerop die-size)
			   (error "Weights sum to zero")
			 (random die-size)))) 
    (goto-char
     (catch 'hit
       (org-child-reduce
	(lambda (acc)
	  (if (>= acc roll) (throw 'hit (point))
	    (let ((val (org-entry-get (point) weight-prop)))
	      (if val (+ acc (string-to-number val))
		acc))))
	0)))))

(provide 'org-spin)
;;; org-spin.el ends here
