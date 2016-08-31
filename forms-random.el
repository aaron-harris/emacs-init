;;; forms-random.el --- Random record selection      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data forms

;; Dependencies: `formation', `forms-narrow', `seq'

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

;; This module provides commands to select a random record in the
;; current `forms-mode' database.
;;
;; The most basic command is `forms-random-record', which selects a
;; record uniformly at random.
;;
;; If you want to the record to be chosen non-uniformly, you can use
;; `forms-random-record-weighted'.  To use this function, set the
;; variable `forms-random-weight-field' to the number of a field you
;; wish to interpret as a weighting factor, and set the variable
;; `forms-random-weight-transform' to be a function that will convert
;; the value in that field into a non-negative integer.
;;
;; Both of these commands support narrowing with the `forms-narrow'
;; module (in the sense that neither will select a record unless it is
;; currently visible).  In addition, there is a command
;; `forms-random-narrow', which narrows the current database to a
;; certain number of randomly-selected records, using the same
;; weighting options as `forms-random-record-weighted'.  It is
;; available on the key `C-r' under the prefix map `forms-narrow-map'.

;;; Code:

(require 'alist)
(require 'formation)
(require 'forms-narrow)
(require 'random)
(require 'seq)


;;;; User Options
;;===============
(defcustom forms-random-narrow-size 10
  "Number of records that `forms-random-narrow' should select by default."
  :group 'forms
  :type  'integer)


;;;; Basic Randomness
;;===================
;;;###autoload
(defun forms-random-record ()
  "Go to a randomly selected record in current database.
If the database is narrowed, respect the current narrowing
predicate."
  (interactive)
  ;; If the database is narrowed, then we have to look at each record
  ;; to see if it's visible.  The logic for this is already in
  ;; `forms-random-record-weighted', so hide the weights and delegate.
  (if (bound-and-true-p forms-narrow-mode)
      (let ((forms-random-weight-field nil))
        (forms-random-record-weighted))
    (forms-jump-record (1+ (random forms--total-records)))))


;;;; Weighted Randomness
;;======================
(defvar-local forms-random-weight-field nil
  "Field number with weights for `forms-random-record-weighted'.
If nil, treat all weights as equal.")

(defun forms-random-weight-transform-default (s)
  "Convert S from a string to a non-negative integer.

This is just the concatenation of the standard functions
`string-to-number', `abs', and `truncate'."
  (truncate (abs (string-to-number s))))

(defvar-local forms-random-weight-transform
  #'forms-random-weight-transform-default
  "Transformation for `forms-random-record-weight'.

The value (a function) is used to prepare the weights for use in
`forms-random-record-weighted'.  It should take a string as an
argument (the value of `forms-random-weight-field' for the
current record) and return a non-negative integer.")

(defun forms-random--get-weight ()
  "Get weight of this record for `forms-random-record-weighted'."
  (if forms-random-weight-field
      (funcall forms-random-weight-transform
               (nth forms-random-weight-field forms-fields))
    1))

(defun forms-random--get-weights ()
  "Return an alist associating weights to record numbers."
  (formation-map
   (lambda () (cons (forms-random--get-weight)
                    forms--current-record))))

;;;###autoload
(defun forms-random-record-weighted ()
  "As `forms-random-record', but die is weighted.

Interpret the field specified by the variable
`forms-random-weight-field' as a weighting factor, using the
value of `forms-random-weight-transform' to transform this into
an integer.

If `forms-random-weight-field' is nil, treat all weights as
equal; i.e., behave identically to `forms-random-record'.

The chance of selecting any particular record R is then n/N,
where n is the value R has for the weighting field and N is the
total of this field across all records in the database."
  (interactive)
  ;; If there are no weights and the database is not narrowed, then
  ;; `forms-random-record' is more efficient, so delegate.
  (if (and (not forms-random-weight-field)
           (not (bound-and-true-p forms-narrow-mode)))
      (forms-random-record)
    (forms-jump-record (random-dispatch* (forms-random--get-weights)))))


;;;; Random Narrowing
;;===================
(defun forms-random-narrow (n &optional verbose)
  "Narrow the current db to show N randomly-selected records.

Use `forms-random-weight-field' and
`forms-random-weight-transform', if those are defined.

The number N of records to select can be supplied via prefix
argument.  If it is omitted, it defaults to the value of the
option `forms-random-narrow-size'."
  (interactive "P")
  (setq n (or n forms-random-narrow-size))
  (when verbose (message "Narrowing to %d random records." n))
  (forms-narrow-widen)
  (let ((record-alist
         (formation-reduce
          (lambda (acc)
            (alist-insert
             acc
             (random (* 10000 (forms-random--get-weight)))
             forms--current-record
             :down)))))
    (forms-narrow (mapcar #'cdr (seq-take record-alist n)))))

(define-key forms-narrow-map (kbd "C-r") #'forms-random-narrow)


;;;; Additional Transforms
;;========================
(defun forms-random-weight-transform-rating (string)
  "Transform a half-point rating into an integer weight.
The transformation is linear (i.e., the numbers are just
doubled).

This function is suitable for use in the variable
`forms-random-weight-transform'."
  (truncate (* 2 (string-to-number string))))

(provide 'forms-random)
;;; forms-random.el ends here
