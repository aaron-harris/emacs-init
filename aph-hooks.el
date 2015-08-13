;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; HOOK FUNCTIONS
;;;;============================================================================

;;; This file contains functions used to add code to elisp hooks,
;;; usually trivial code that wouldn't need a name otherwise.


;;; Adding Hooks
;;;=============
(defun aph/add-hook-safely (hook function &optional append local)
  "As `add-hook', but check if FUNCTION is defined first.

If FUNCTION is defined, proceed to add it to HOOK. If it isn't,
print a message to that effect and do nothing.

The return value is HOOK if it was added and nil otherwise."
  (if (fboundp function)
      (add-hook hook function append local)
    (message "Cannot add #'%s to %s: Function not defined"
             function hook)
    nil)) 

(defun aph/add-hook-to-all (hooks function &optional safely)
  "Add FUNCTION to each hook in the list HOOKS, with `add-hook'.

If the optional parameter SAFELY is supplied, use
`aph/add-hook-safely' instead of `add-hook'."
  (dolist (hook hooks)
    (if safely
        (aph/add-hook-safely hook function)
      (add-hook hook function))))


;;; Hook Code
;;;==========
(defun aph/truncate-lines-on ()
  "Cause current buffer to truncate long lines."
  (toggle-truncate-lines 1))

(defun aph/truncate-lines-off ()
  "Cause current buffer to fold long lines."
  (toggle-truncate-lines -1))

(provide 'aph-hooks)
