;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================

(defun aph/random-comparator (x y)
  "Randomly returns +1 with probability 1/2 and -1 with probability 1/2. Ignores
its arguments.

Intended for use as a comparator in a sorting mechanism. When used in such a
way, the results will be shuffled (sorted randomly)."
  (truncate (* 2 (- (random* 2) 0.5))))
