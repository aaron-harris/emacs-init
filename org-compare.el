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
;;
;; `org-compare-randomly'
;;
;;     This meta-comparator implements a random shuffle.  Care is
;;     taken to make sure the shuffle is fully random, and options are
;;     provided both to make refreshing the agenda preserve the order,
;;     and to make a refresh trigger a reshuffle.
;;
;; `org-compare-randomly-by'
;;
;;     As `org-compare-randomly', except the user can supply a
;;     function to generate random keys, allowing for a non-uniform
;;     distribution.

;;; Code:

(require 'org)

(eval-when-compile (require 'dash))
(require 'seq)


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

(defun org-compare--get-marker (entry)
  "Return the marker for ENTRY.

This marker points to the location of the headline referenced by
ENTRY."
  (get-text-property 1 'org-marker entry))


;;;; Property Comparators
;;=======================
(defun org-compare--get-property (entry prop &optional numeric default)
  "Return the value of PROP in ENTRY.

Here PROP is a string denoting an Org-mode property name
and ENTRY is an Org-mode agenda entry.

If the optional parameter NUMERIC is non-nil, return the value as
a number.

Normally, if ENTRY does not have a value for PROP, return nil.
If the optional parameter DEFAULT is supplied, instead return
that value."
  (let ((raw-val (org-entry-get (org-compare--get-marker) prop)))
    (cond
     ((null raw-val)  (or default nil))
     (numeric         (string-to-number raw-val))
     (t                raw-val))))

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


;;;; Random Comparators
;;=====================
(defvar org-compare-random-refresh nil
  "Whether `org-compare-randomly' should refresh its keys.

See the docs for `org-compare-randomly' for more information.")

(defun org-compare-randomly--update-sort-key (entry table generator)
  "Return sort key for ENTRY in TABLE, generating it if necessary.
For internal use by `org-compare-randomly-by'."
  (let* ((marker    (org-compare--get-marker entry))
         (hash-key  `(,(marker-buffer marker) . ,(marker-position marker))))
    (or (gethash hash-key table)
        (puthash hash-key (funcall generator entry) table))))

(defun org-compare-randomly-by (generator)
  "Return a random comparator using GENERATOR.

The comparator returned is like `org-compare-randomly', except
the distribution of random keys is controlled by GENERATOR and
may thus be non-uniform.

The function GENERATOR is called with a single argument, an
agenda entry, when that entry lacks a sort key.  It should return
a number, which is then used for all comparisons until the key
list is cleared; see `org-compare-randomly' for more details on
this.

Subsequent calls to `org-compare-randomly-by' produce comparators
with independent sets of sort keys."
  (let ((table (make-hash-table :test #'equal)))
    (lambda (x y) 
      (when org-compare-random-refresh 
        (clrhash table)
        (setq org-compare-random-refresh nil))
      (let ((x-val (org-compare-randomly--update-sort-key x table generator))
            (y-val (org-compare-randomly--update-sort-key y table generator)))
        (cond
         ((= x-val y-val)  nil)
         ((< x-val y-val)   -1)
         ((> x-val y-val)   +1))))))

(defun org-compare-randomly ()
  "Return a comparator implementing a random shuffle.

When given distinct agenda entries X and Y, the resulting
comparator has an equal chance of returning +1 and -1 (and a
miniscule chance of returning nil).  Subsequent calls will result
in results consistent with a total ordering.

To accomplish this, a hash table of randomly-generated sort keys
is maintained.  This table will persist until the comparator is
called when the variable `org-compare-random-refresh' is non-nil.
This means that setting this variable as part of a custom agenda
command using this comparator as `org-agenda-cmp-user-defined'
will cause the sort order to change whenever the agenda is
refreshed; otherwise, it will persist until Emacs is restarted.

Note that if you don't want the sort order to change on refresh,
you need to be careful that the comparator is created when the
custom agenda command is defined, not when it's called, e.g.

    (add-to-list
     'org-agenda-custom-commands
     `(\"y\" \"Example Agenda\"
       ((todo
         \"\"
         ((org-agenda-cmp-user-defined ',(org-compare-randomly))
          (org-agenda-sorting-strategy '(user-defined-up)))))))

\(Notice the use of backquote.)

Comparators resulting from different calls to this function have
independent key tables."
  (org-compare-randomly-by (lambda (_) (random))))

(provide 'org-compare)
;;; org-compare.el ends here
