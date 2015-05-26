;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================

(defun aph/add-hook-to-all (hooks function)
  "Add FUNCTION to each element of the list HOOKS."
  (dolist (hook hooks)
    (add-hook hook function)))

(defun aph/define-keys (keymap binding-list)
  "For each pair (KEY . DEF) in BINDING-LIST, define the key
sequence KEY as DEF in KEYMAP.

See `define-key' for more information. Unlike `define-key', we
also accept for KEY any lisp code that evaluates to a valid key
sequence (e.g., a quoted call to `kbd')."
  (dolist (binding binding-list)
    (define-key keymap (eval (car binding)) (eval (cdr binding)))))

(defvar aph/fill-column-by-mode-alist '()
  "An alist mapping modes to the desired values of `fill-column'
  within those modes. Used by `aph/fill-set-column-by-mode' to
  set `fill-column' appropriately, usually within a mode hook.")

(defun aph/fill-set-column-by-mode ()
  "Set `fill-column' to the value corresponding to the current
mode in `aph/fill-column-by-mode-alist.' If the current mode does
not appear in that list, do nothing."
  (let ((val (cdr (assq major-mode aph/fill-column-by-mode-alist))))
    (if val (setq fill-column val))))

(defun aph/random-comparator (&rest args)
  "Randomly return +1 with probability 1/2 and -1 with
probability 1/2. Ignore any ARGS.

Intended for use as a comparator in a sorting mechanism. When
used in such a way, the results will be shuffled (sorted
randomly)."
  (truncate (* 2 (- (random 2) 0.5))))
