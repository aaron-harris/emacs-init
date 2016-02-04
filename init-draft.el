;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.


;;; Augmented Keymaps
;;;==================
(bind-keys (:map (aph-keys-augment-major-mode-map 'org-mode)
                 ("M-." . org-goto)))

(require 'aph-mode-tag-test)            ; For `aph/with-test-mode'

(defun aph-keys-augment-major-mode-map (mode)
  "Return the keymap for MODE augmented for `aph-keys-mode'.

The parameter MODE should be a symbol representing a major mode.
The returned keymap inherits from the keymap for MODE and may
also include its own bindings.  It will be active only when both
MODE (or a mode descended from MODE) and `aph-keys-mode' are
active and provides a mechanism for mode-local keybindings that
are still toggleable with `aph-keys-mode'.

Subsequent calls to this function for the same MODE return the
same keymap, including any bindings that were made to that keymap
after its construction.  That is, there is at most one augmented
keymap for each mode, and this function returns that keymap,
constructing it if necessary.

Because the mechanism used to determine whether such an augmented
keymap will be active uses mode hooks, an augmented keymap for
`fundamental-mode' will not work as expected and should probably
be avoided.")

;; Thoughts:
;; - How am I storing the augmented keymap?  Probably with a symbol
;; property on MODE-map (e.g., the symbol `org-mode-map').
;; - I think I probably need to tinker with `aph/with-test-mode'.  At
;; the very least this should be renamed and moved out of
;; `aph-mode-tag-test' (possibly rename to `aph/ert-with-test-mode'
;; and move to `aph-ert'?).  Also I will probably need to add the
;; possibility of a keymap binding.


(provide 'init-draft)
