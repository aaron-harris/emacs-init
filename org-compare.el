;;; org-compare.el --- Comparators for Org mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: calendar

;; Dependencies: `org', `seq'

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

;; This module contains extra comparators that can be used with
;; `org-agenda-cmp-user-defined' to implement custom sorting behavior
;; in `org-mode' custom agenda commands.
;;
;; Note that some of these functions can be used as comparators
;; directly, while others are "meta-comparators" that take in some
;; information and return a function usable as a comparator.
;;
;; Included comparators and meta-comparators are as follows:
;;
;; `org-compare-property'
;;
;;     This meta-comparator returns a comparator that sorts entries
;;     based on their values for a specific property.  Options are
;;     provided for both string and numeric comparisons, as well as
;;     custom user-supplied transforms.

;;; Code:

(require 'org)

(eval-when-compile (require 'dash))
(require 'seq)


;;;; User Options
;;===============
(defgroup org-compare nil
  "Comparators for the Org agenda."
  :prefix "org-compare-"
  :link   '(emacs-commentary-link "org-compare")
  :group  'org-agenda)


;;;; Subroutines
;;==============
(defun org-compare-strings (str1 str2 &optional ignore-case)
  "As `compare-strings' but return +1, -1, or nil.

Compare strings X and Y.  If X is less than Y (in the sensse of
`compare-strings'), return -1.  If Y is less than X, return +1.
If the strings are equal, return nil.

Unlike `compare-strings' substrings cannot be compared.

Note that this function is not itself suitable for use as an
agenda comparator (see `org-agenda-cmp-user-defined'), since it
compares strings and not agenda entries."
  (let ((compare (compare-strings str1 nil nil str2 nil nil ignore-case)))
    (cond
     ((eq t compare) nil)
     ((> compare 0)  +1)
     ((< compare 0)  -1))))

(defun org-compare--get-property (entry prop &optional numeric default)
  "Return the value of PROP in ENTRY.

Here PROP is a string denoting an Org-mode property name
and ENTRY is an Org-mode agenda entry.

If the optional parameter NUMERIC is non-nil, return the value as
a number.

Normally, if ENTRY does not have a value for PROP, return nil.
If the optional parameter DEFAULT is supplied, instead return
that value."
  (let ((raw-val
         (-> (get-text-property 1 'org-marker entry)
             (org-entry-get prop))))
    (cond
     ((null raw-val)  (or default nil))
     (numeric         (string-to-number raw-val))
     (t                raw-val))))


;;;; Comparators
;;==============
(defun org-compare-property (prop type &optional default transform)
  "Return a comparator that sorts by PROP.

The returned function takes two arguments (`org-mode' agenda
entries).  It compares the two entries (in a manner suitable for
use in `org-agenda-cmp-user-defined') based on their values for
PROP.

The parameter TYPE specifies what type of comparison to use and
should be one of the following keywords:

:number            Numeric comparison
:string            String comparison, case-sensitive
:string-no-case    String comparison, case-insensitive
:transform         Numeric comparison, no conversion (see below)

If the optional parameter DEFAULT is supplied, it will be used
for entries which lack a value for PROP.  Otherwise, such entries
will be sorted last.

If the second optional parameter TRANSFORM is supplied, it should
be a function that is applied to the parameter values before
comparison.  If TYPE is :number, the values are coerced to
numbers before TRANSFORM is applied.

If TYPE is :transform, then the values are compared as numbers
but are not coerced to numbers.  In this case, TRANSFORM has the
responsibility of converting the values to numbers itself.  If it
does not, an error will be signaled." 
  (let ((transform (or transform #'identity)))
    (lambda (x y)
      (let ((x-val
             (->> (org-compare--get-property x prop (eq type :number) default)
                  (funcall transform)))
            
            (y-val
             (->> (org-compare--get-property y prop (eq type :number) default)
                  (funcall transform))))
        (cond
         ;; Nil handling
         ((and (null x-val) (null y-val))  nil)
         ((null x-val)                     -1)
         ((null y-val)                     +1) 
         ;; String comparison
         ((seq-contains '(:string :string-no-case) type #'eq)
          (org-compare-strings x-val y-val (eq type :string-no-case)))
         ;; Numeric comparison
         ((= x-val y-val)  nil)
         ((< x-val y-val)  -1)
         ((> x-val y-val)  +1))))))

(provide 'org-compare)
;;; org-compare.el ends here
