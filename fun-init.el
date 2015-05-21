;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================

(defun aph/add-hook-to-all (hooks function)
  "Adds FUNCTION to each element of the list HOOKS."
  (mapcar (lambda (hook) (add-hook hook function)) hooks))

(defun aph/random-comparator (&optional args)
  "Randomly returns +1 with probability 1/2 and -1 with probability 1/2. Ignores
its arguments.

Intended for use as a comparator in a sorting mechanism. When used in such a
way, the results will be shuffled (sorted randomly)."
  (truncate (* 2 (- (random 2) 0.5))))
