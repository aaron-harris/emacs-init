;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG MODE AGENDA SUBROUTINES
;;;;============================================================================

;;; This file contains functions which are useful in building custom
;;; agenda commands.

;; Some functions in this file require the 'aph-comparators library at runtime.


;;; Information Extraction
;;;=======================
;; Functions in this section extract useful information from agenda
;; entries.  Such entries are passed to custom comparator functions,
;; among other uses.
(defun aph/org-get-property (entry prop &optional numeric default)
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
(defun aph/org-compare-strings (str1 str2 &optional ignore-case)
  "As `compare-strings' but return +1, -1, or nil.

Compare strings X and Y.  If X is less than Y (in the sensse of
`compare-strings'), return -1.  If Y is less than X, return +1.
If the strings are equal, return nil.

Unlike `compare-strings' substrings cannot be compared.

This function is intended for use with
`org-agenda-cmp-user-defined', but cannot be used directly, since
it takes strings and not agenda entries."
  (let ((compare (compare-strings str1 nil nil str2 nil nil ignore-case)))
    (cond
     ((eq t compare) nil)
     ((> compare 0)  +1)
     ((< compare 0)  -1))))

(defun aph/org-property-comparator-maker (prop type &optional default transform)
  "Return a comparator that sorts by PROP.

The returned function takes two arguments (Org-mode agenda
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
comparison.  If TYPE is :numeric, the values are coerced to
numbers before TRANSFORM is applied.

If TYPE is :transform, then the values are compared as numbers
but are not coerced to numbers.  In this case, TRANSFORM has the
responsibility of converting the values to numbers itself.  If it
does not, an error will be signaled." 
  (let ((transform (or transform #'identity)))
    (lambda (x y)
      (let ((x-val  (->> (aph/org-get-property x prop (eq type :number) default)
                         (funcall transform)))
            (y-val  (->> (aph/org-get-property y prop (eq type :number) default)
                         (funcall transform))))
        (cond
         ;; Nil handling
         ((and (null x-val) (null y-val))  nil)
         ((null x-val)                     -1)
         ((null y-val)                     +1) 
         ;; String comparison
         ((-contains-p '(:string :string-no-case) type)
          (aph/org-compare-strings x-val y-val (eq type :string-no-case))) 
         ;; Numeric comparison
         ((= x-val y-val)  nil)
         ((< x-val y-val)  -1)
         ((> x-val y-val)  +1))))))

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
