;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================

(defun aph/add-hook-to-all (hooks function)
  "Adds FUNCTION to each element of the list HOOKS."
  (mapcar (lambda (hook) (add-hook hook function)) hooks))

(defvar aph/fill-column-by-mode-alist '()
  "An alist mapping modes to the desired values of fill-column
  within those modes. Used by aph/fill-set-column-by-mode to set
  fill-column appropriately, usually within a mode hook.")

(defun aph/fill-set-column-by-mode ()
  "Set fill-column to the value corresponding to the current mode
in aph/fill-column-by-mode-alist. If the current mode does not
appear in that list, reset fill-column to the default."
  (let ((val (cdr (assq major-mode aph/fill-column-by-mode-alist))))
    (if val (set-fill-column val))))

(defun aph/random-comparator (&rest args)
  "Randomly returns +1 with probability 1/2 and -1 with probability 1/2. Ignores
its arguments.

Intended for use as a comparator in a sorting mechanism. When used in such a
way, the results will be shuffled (sorted randomly)."
  (truncate (* 2 (- (random 2) 0.5))))
