;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; COMPARATOR FUNCTIONS
;;;;============================================================================

;;; This file contains comparator functions, particularly
;;; non-deterministic comparators that can be used to induce a random
;;; ordering.

;; Some functions in this file require the 'cl-lib library at runtime.


;;; Basic Comparators
;;;==================
(defun aph/random-comparator (&rest args)
  "Randomly return +1 or -1.

Randomly return +1 with probability 1/2 and -1 with
probability 1/2. Ignore any ARGS.

Intended for use as a comparator in a sorting mechanism. When
used in such a way, the results will be shuffled (sorted
randomly)."
  (truncate (* 2 (- (random 2) 0.5))))


;;; Metacomparators
;;;================
;; The functions in this section are not themselves comparators, but
;; instead build a comparator around their arguments.
(defun aph/weighted-comparator-maker (bias-fn)
  "Return a weighted comparator using BIAS-FN.

The returned function takes two arguments and applies BIAS-FN (a
function that takes a single argument and returns a number
indicating the relative preference for that argument) to each of
them. The difference of the results is added to 0.5, restricted
to the interval \\[0,1], and interpreted as the probability that
the function should return +1 rather than -1.

If BIAS-FN returns a non-numeric result for either argument, the
comparison is unbiased (the difference is taken to be 0.5)."
  (require 'cl-lib)                       ; for `cl-random'
  (lambda (x y)
    (let* ((bias-x  (funcall bias-fn x))
           (bias-y  (funcall bias-fn y))
           (bias    (if (and (numberp bias-x)
                             (numberp bias-y))
                        (+ 0.5 (- bias-x bias-y))
                      0.5))
           (roll    (cl-random 1.0)))
      (if (< roll bias) +1 -1))))

(provide 'aph-comparators)
