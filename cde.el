;;; cde.el --- Count an elided sequence              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience, local

;; Dependencies: `dash'

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

;; This module introduces a function `cde'.  This function takes a
;; elided sequence of integers in string form and counts how many
;; integers that sequence represents.  (The name stands for "count,
;; de-elide").
;;
;; What's an "elided sequence"?  (And yes, I think that I coined the
;; term myself.)  An elided sequence is a comma- and hyphen- delimited
;; string of integers, like you might see in the index of a text book
;; telling you which pages a particular term appears on.  For example,
;; "1,3-7,10" is an elided sequence.
;;
;; More specifically, an elided sequence is a comma- delimited string
;; whose elements are called "ranges".  A lone integer is a range, and
;; it just represents itself.  Two integers separated by a hyphen are
;; also a range; this represents the set of all integers between and
;; including the two endpoints.
;;
;; If an elided sequence consists of disjoint ranges, and each range
;; is non-decreasing (that is, the first endpoint of a range is not
;; larger than the second), then `cde' returns the number of integers
;; in the union of the sets represented by those ranges.  Note that it
;; does not verify these preconditions and may not give you a
;; meaningful answer if they are violated.
;;
;;
;; Extra features:
;;
;; - `cde' will accept lists, too.  For instance, the elided sequence
;;   "1-5,7,8-15" could be represented as the list '((1 5) 7 (8 15)).
;;   This exists mainly because `cde' uses lists internally, so
;;   providing list support requires no effort.
;;
;; - `cde' can interpret ranges whose endpoints are of the form "nX",
;;   where n is an integer and X is an alphabetic character.  If X is
;;   the character "B", then this is interpreted as the integer n+p,
;;   where p is the value of the variable `cde-page-size'; otherwise,
;;   X is ignored and nX is treated as n.
;;
;;   The idea is that a range of this form could be used to denote
;;   lines on a page, where each side of the page is numbered from 1
;;   to `cde-page-size'.  For example, "1A-6B,13B-17B" would include
;;   all lines on the front, the first six lines on the back, and
;;   lines 13-17 on the back.
;;
;; - The command `cde-format' provides an interactive front-end to
;;   `cde'.  This command attempts to interpret the text under point
;;   as an elided sequence.  If it succeeds, it will wrap it in
;;   parentheses (unless it's already so wrapped) and prepend the
;;   result of calling `cde' on it.

;;; Code:
(require 'dash)


;;;; Configuration Variables
;;;;========================
(defvar cde-page-size 50
  "The increment size for alphabetic characters in `cde'.
See the documentation of `cde' for more information.")


;;;; Subroutines
;;;;============
(defun cde--unpage (ref)
  "Convert REF to an integer using `cde-page-size'.
Here REF should be a string of the form \"nX\", where n is an
integer and X is any string.

The return value is n unless X is the string \"B\", in which case
it is n plus the value of `cde-page-size'.

This function is used as a subroutine by `cde'."
  (save-match-data
    (string-match "\\([0-9]+\\)\\(B?\\)" ref)
    (let ((n  (string-to-int (match-string 1 ref)))
          (x  (match-string 2 ref)))
      (+ n (if (equal x "B") cde-page-size 0)))))

(defun cde--list (ranges)
  "Count the numbers in RANGES.

As `cde', but RANGES must be in list form."
  (->> ranges
       (mapcar (lambda (elt) 
                 (if (and (consp elt) (cdr elt))
                     (- (cadr elt) (1- (car elt)))
                   1)))
       (apply #'+)))

(defun cde--string (ranges)
  "Count the numbers in RANGES.

As `cde', but RANGES must be in string form."
  (save-match-data
    (->> (split-string ranges ",")
         (mapcar (lambda (range)
                   (->> (split-string range "-") 
                        (mapcar #'cde--unpage))))
         (cde--list))))


;;;; The main function
;;;;==================
;;;###autoload
(defun cde (ranges)
  "Count the numbers in RANGES.

Here, RANGES may either be a comma-separated string of hyphenated
ranges, e.g. \"1-5,7,8-15\", or a list encoding the same
information, e.g., '((1 5) 7 (8 15)). For both of the examples above,
cde will return 14.

Additionally, the alphabetic character \"A\" and \"B\" are
interpreted as sides of a page, with the page size given by
`cde-page-size'.  Thus if `cde-page-size' is 50 (the default),
then the string \"5A\" will be interpreted as the integer 5,
while the string \"5B\" will be interpreted as 55.  This feature
is unavailable if RANGES is presented as a list."
  (cond
   ((listp ranges)    (cde--list ranges))
   ((stringp ranges)  (cde--string ranges))))


;;;; Interactive front-ends
;;;;=======================
;;;###autoload
(defun cde-format (&optional verbose)
  "Convert word at point with `cde'.

If the text at point looks like a suitable input for `cde',
replace it with a string of the form

    N (RANGES)

where RANGES is the original word and N is the result of calling
`cde' on RANGES.  The return value is the text inserted.

If the text at point is not suitable for `cde', do nothing and
return nil.  Interactively, or with VERBOSE non-nil, print an
explanatory message."
  (interactive "p")
  (save-match-data
    (cond
     ;; On successful match, do the replacement
     ((thing-at-point-looking-at
       "(?\\(\\(?:[0-9]+[AB]?[,-]?\\)*[0-9]+[AB]?\\))?\\(,?\\)")
      (let ((ranges  (match-string 1))
            (tail    (match-string 2)))
        (replace-match (format "%d (%s)%s" (cde ranges) ranges tail))))
     ;; Otherwise, maybe display a message.
     (verbose
      (message "Cannot use `cde' at point")
      nil))))

(provide 'cde)
;;; cde.el ends here
