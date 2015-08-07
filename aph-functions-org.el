;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE
;;;;============================================================================

(require 'org)

(require 'dash)                         ; for `->>', `-find-index'
(require 'aph-functions)                ; for `aph/reductions',
                                        ;  `aph/weighted-comparator-maker'


;;; Heading Structure
;;;==================
(defun aph/org-count-children (&optional pom)
  "Return the number of children for Org mode heading at POM.

If POM (a number or a marker) is omitted, use point. If POM is
not in a heading, return nil."
  (let ((pom (or pom (point))))
    (save-excursion
      (goto-char pom)
      (unless (org-before-first-heading-p)
        (org-back-to-heading :invisible-ok)
        (if (org-goto-first-child)
            (let ((acc  1))
              (while (org-get-next-sibling)
                (setq acc (1+ acc)))
              acc)
          0)))))

(defun aph/org-goto-nth-child (n)
  "Goto the Nth child of heading at point.

Children are counted from 1. If heading does not have N children,
return nil and do not move point; otherwise, return point.

If N is zero, call `org-back-to-heading' and return point.

If N is negative, goto the (-N)th child from the end (so
(aph/org-goto-nth-child -1) moves to the last child)."
  (cond ((zerop n) (progn (org-back-to-heading)
                        (point))) 
        ((< n 0)   (aph/org-goto-nth-child
                  (+ (aph/org-count-children) (1+ n)))) 
        ((> n 0)
         (let ((target (catch 'fail
                         (save-excursion
                           (unless (org-goto-first-child)
                             (throw 'fail nil))
                           (dotimes (i (1- n) (point))
                             (unless (org-get-next-sibling)
                               (throw 'fail nil)))))))
           (when target
             (goto-char target))))))


;;; Properties
;;;===========
(defun aph/org-get-property-of-children
    (pom prop &optional inherit literal-nil)
  "Return list of PROP values for all children of heading at POM.
See `org-entry-get' for use of optional parameters."
  (save-excursion
    (goto-char pom)
    (unless (org-goto-first-child) '())
    (let ((acc (list (org-entry-get (point) prop inherit literal-nil))))
      (while (org-get-next-sibling)
        (push (org-entry-get (point) prop inherit literal-nil) acc))
      (nreverse acc))))

(require 'dash)                         ; for `->>'
(defun aph/org-sum-property-of-children
    (pom prop &optional inherit)
  "Return sum of PROP values for all children of heading at POM.

If INHERIT is non-nil, use inherited values for PROP. Ignore
non-numeric values."
  (->> (aph/org-get-property-of-children pom prop inherit)
       (mapcar #'string-to-number)
       (apply #'+)))


;;; Spinners
;;;=========
(defun aph/org-spin-basic ()
  "Move point to a random child of heading at point.
Return point."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Point not on heading.")
    (let ((die-size  (aph/org-count-children)))
      (aph/org-goto-nth-child (1+ (random die-size))))))

(defvar aph/org-spin-weight-property
  "Weight"
  "The default property to be used for `aph/org-spin-weight'.")

(defun aph/org-spin-weighted (&optional weight-prop)
  "As `aph/org-spin-basic', weighted by property WEIGHT-PROP.

The parameter WEIGHT-PROP should be the name of a property.
Non-negative numeric values for that property are treated as
weights for the spin. Non-numeric and negative values are treated
as zero.

When called interactively or if WEIGHT-PROP is
omitted,`aph/org-spin-weight-property' is used."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Point not on heading.")
    (org-back-to-heading))
  (let* ((weight-prop  (or weight-prop aph/org-spin-weight-property))

         (weight-list
          (->> (aph/org-get-property-of-children (point) weight-prop)
               (mapcar #'string-to-number)
               (mapcar (lambda (x) (if (< x 0) 0 x)))))
         
         (threshold-list  (aph/reductions #'+ weight-list)) 
         (roll            (random (apply #'+ weight-list)))) 
    (->> threshold-list
         (-find-index (apply-partially #'< roll))
         1+
         aph/org-goto-nth-child)))


;;; Agenda
;;;=======
(defun aph/org-comparator-rating-to-weight-default (rating)
  "The default for `aph/org-comparator-rating-weight-function'.

This transformation is linear, mapping \\[0,5] onto
\\[-0.125,0.125]. Since larger values are typically interpreted
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
  (let* ((rating-prop (or rating-prop aph/org-rating-property)) 
         (bias-fn
          (lambda (item)
            (--> (get-text-property 1 'org-marker item)
                 (org-entry-get it aph/org-rating-property)
                 (string-to-number it)
                 (funcall aph/org-comparator-rating-to-weight it)))))
    (funcall (aph/weighted-comparator-maker bias-fn) x y)))

(provide 'aph-functions-org)
