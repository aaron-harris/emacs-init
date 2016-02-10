;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; PERSONAL KEYBINDING MODE
;;;;============================================================================

;; A minor mode for implementing my personal keybindings, along with
;; some other utilities related to keybindings.


;;; `aph-keys-mode': Mode basics
;;;=============================
(defvar aph-keys-mode-map (make-sparse-keymap)
  "Global keymap for `aph-keys-mode'.
To bind a key in `aph-keys-mode' conditionally on a major or
minor mode, use `aph-keys-augment'.")

(define-minor-mode aph-keys-mode
  "Mode for the personal keybindings of Aaron Harris."
  :global  t
  :lighter " #")


;;; `aph-keys-mode': Augmented keymaps
;;;===================================
(defun aph-keys--augment-name (mode)
  "Return the name of the augmented keymap for MODE.

The returned symbol is the name of the variable that would
contain the augmented keymap for MODE, if one exists.  See
`aph-keys-augment' for more information."
  (aph/symbol-concat 'aph-keys-mode-map (format ":%s" mode)))

(defun aph-keys-augmented-p (mode)
  "Return non-nil if MODE has an augmented keymap.
See `aph-keys-augment' for more information."
  (let ((augmap  (aph-keys--augment-name mode)))
    (boundp augmap)))

(defmacro aph-keys-augment--define (mode)
  "Define augmented keymap variable for MODE.
Used by `aph-keys-augment'.  See that function for more
details."
  (declare (debug (symbolp)))
  (let ((augmap (aph-keys--augment-name mode)))
    `(defvar ,augmap (make-sparse-keymap)
       ,(format (concat "Augmented keymap for `%s'.\n"
                        "See `aph-keys-augment' for more details.")
                mode))))

(defun aph-keys-augment-var (mode)
  "As `aph-keys-augment', but return a variable.
This variable contains the keymap that would be returned by
`aph-keys-augment'."
  (if (aph-keys-augmented-p mode)
      (aph-keys--augment-name mode)
    (eval `(aph-keys-augment--define ,mode))))

(defun aph-keys-augment (mode)
  "Return augmented keymap corresponding to MODE for `aph-keys-mode'.

The parameter MODE should be a symbol naming a major or minor
mode (e.g., the symbol 'text-mode-map).  Other types of variables
can be passed, but this serves no purpose, since augmented
keymaps created in this way will not be automatically activated.

The keymap returned will be active whenever both MODE (or a mode
descended from MODE) and `aph-keys-mode' are active (once this
feature, currently under construction, is complete).  This
provides a mechanism for mode-local keybindings that are still
toggleable with `aph-keys-mode'.

Subsequent calls to this function with the same argument return
the same keymap, including any bindings that were made to that
keymap after its construction.  That is, there is at most one
augmented keymap for each mode, and this function returns that
keymap, constructing it if necessary.

The variable holding an augmented keymap is named using the
format 'aph-keys-mode-map:MODE', e.g. 'aph-keys-mode:text-mode'.

Because the mechanism used to determine whether such an augmented
keymap will be active uses mode hooks, an augmented keymap for
`fundamental-mode' will not work as expected and should probably
be avoided."
  (symbol-value (aph-keys-augment-var mode)))


;;; `aph-keys-mode': Mode-specific keybindings
;;;===========================================
(defun aph-keys--update-major-mode ()
  "Update `aph-keys-mode-map' for current major mode.
Set `aph-keys-mode-map' to the augmented keymap for the current
major mode, if one exists, and `aph-keys-mode-global-map'
otherwise.  See `aph-keys-augment' for more information.

This function is suitable for use in
`after-change-major-mode-hook'."
  (let ((keymap 
         (if (aph-keys-augmented-p major-mode)
             (let ((augmap (aph-keys-augment major-mode))) 
               (unless (keymap-parent augmap)
                 (set-keymap-parent augmap aph-keys-mode-map))
               augmap)
           aph-keys-mode-map)))
    (make-local-variable 'minor-mode-map-alist)
    (push `(aph-keys-mode . ,keymap) minor-mode-map-alist)))

(add-hook 'after-change-major-mode-hook #'aph-keys--update-major-mode)


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
