;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; PERSONAL KEYBINDING MODE
;;;;============================================================================

;; A minor mode for implementing my personal keybindings, along with
;; some other utilities related to keybindings.


;;; Personal Keybinding Mode
;;;=========================
(defvar aph-keys-mode-map 
  (make-sparse-keymap)
  "Keymap for `aph-keys-mode'.")

(define-minor-mode aph-keys-mode
  "Mode for the personal keybindings of Aaron Harris."
  :global  t
  :lighter " #")


;;; Key Translation
;;;================
;; Functions in this section deal with translation keymaps.

(defun aph/keys-liberate-escape (&optional frame)
  "Decouple the escape key from its ASCII roots.

By default, the escape key and `C-[' resolve to the same
key (character code 27), even in a GUI.  This function breaks
that association (using `input-decode-map') so that the `C-[' key
can be used for other things.

After this function has been executed, the escape key will
continue to resolve to character code 27, but `C-[' will resolve
to the bare symbol `C-['.  This means that the relationship
between the escape key and the meta key is unaffected, but to
bind a key to `C-[' you should use (kbd \"<C-[>\") rather
than (kbd \"C-[\").

To make the change effective when using emacsclient, add this
function to `after-make-frame-functions'."
  (with-selected-frame (or frame (selected-frame))
    (define-key input-decode-map (kbd "C-[") (kbd "<C-[>"))))


(provide 'aph-keys)
