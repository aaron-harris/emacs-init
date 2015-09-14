;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG MODE AGENDA SUBROUTINES
;;;;============================================================================

;;; This file contains functions which are useful in building custom
;;; agenda commands.

;; Some functions in this file require the 'aph-comparators library at runtime.


;;; Skip Functions
;;;===============
;;; These are functions to be used with `org-agenda-skip-function'.
;;; The initial structure was taken from a stackexchange question, written by
;;; user Jonathan Leech-Pepin.

;; TODO: These functions are still a little quirky. Specifically, they only seem
;;       to work properly when the headline they're called on is visible. Until
;;       this is sorted out, all agenda files should have their default
;;       visibility setting set to CONTENTS or higher.
(defun aph/org-agenda-skip-tag (tag)
  "Skip current headline if it is tagged with TAG.

Return nil if headline containing point is tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines tagged with TAG (including by
inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                                  (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        (1- next-headline)
      nil)))

(defun aph/org-agenda-skip-without-tag (tag)
  "Skip current headline unless it is tagged with TAG.

Return nil if headline containing point is not tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines not tagged with TAG (including by
inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                             (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        nil
      (1- next-headline))))


;;; Comparators
;;;============
(defun aph/org-comparator-rating-to-weight-default (rating)
  "The default for `aph/org-comparator-rating-weight-function'.

This transformation is linear, mapping \[0,5\] onto
\[-0.125,0.125\]. Since larger values are typically interpreted
as better, use a descending sort to sort better values to the
top."
  (/ (- rating 2.5) 20.0))

(defvar aph/org-comparator-rating-to-weight
  #'aph/org-comparator-rating-to-weight-default
  "A function transforming a 0-5 rating to a coinflip bias.

Used with `aph/weighted-comparator-maker' in
`aph/org-rating-comparator'.

This function should take in a number in the range
\\[0,5] (representing a subjective \"5-star\" rating) and rescale
that number for use as a coinflip bias, as described in
`aph/weighted-comparator-maker'.")

(defvar aph/org-rating-property
  "Rating"
  "The name of the Org mode property used to store ratings.
Used in `aph/org-rating-comparator'.")

(defun aph/org-rating-comparator (x y &optional rating-prop)
  "Randomly return +1 or -1, biased by ratings of arguments.
Intended for use in a user-defined sorting strategy in an
Org-mode agenda (see `org-agenda-cmp-user-defined').

The arguments X and Y are Org-mode agenda entries, and the bias
on the coinflip is determined by the application of the function
in `aph/org-comparator-rating-to-weight' to the values X and Y
have for the property RATING-PROP (which is given by
`aph/org-rating-property' when omitted).

See `aph/weighted-comparator-maker' for more information."
  (require 'aph-comparators) ; For `aph/weighted-comparator-maker'
  (let* ((rating-prop (or rating-prop aph/org-rating-property)) 
         (bias-fn
          (lambda (item)
            (--> (get-text-property 1 'org-marker item)
                 (org-entry-get it aph/org-rating-property)
                 (string-to-number it)
                 (funcall aph/org-comparator-rating-to-weight it)))))
    (funcall (aph/weighted-comparator-maker bias-fn) x y)))

(provide 'aph-org-agenda)
