;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; PERSONAL KEYBINDING MODE
;;;;============================================================================

;; A minor mode for implementing my personal keybindings, along with
;; some other utilities related to keybindings.

(require 'cl-lib)                       ; For `cl-pushnew'


;;; `aph-keys-mode': Keymap Setup
;;;==============================
(defvar aph-keys-augment-map-alist nil
  "Alist of augmented keymaps for `aph-keys-mode'.
This alist contains all keymaps augmented for use by
`aph-keys-mode'.  See the function `aph-keys-augment' for more
information.")

(defvar-local aph-keys-minor-mode-map-alist nil
  "Alist of active minor mode keymaps for `aph-keys-mode'.
For use in `emulation-mode-map-alists'.

Note that augmented keymaps for major modes will appear in this
list, but their presence does nothing since there is no
corresponding control variable for a major mode.  Major mode
bindings are instead handled by the function
`aph-keys--update-major-mode'.")

(defvar-local aph-keys-local-map-alist
  `((aph-keys-mode . ,(make-sparse-keymap)))
  "An alist holding augmented keymap for current major mode.

This alist should contain a single element of the
form ('aph-keys-mode . KEYMAP), where KEYMAP is the augmented
keymap corresponding to the current major mode.  See
`aph-keys-augment' for more information about augmented keymaps.

This variable is intended for use in `emulation-mode-map-alists'.
It is separate from `aph-keys-augment-map-alist' so that it can
be buffer-local and thus vary with the buffer's major mode.

The association between this variable and the major mode is
maintained by the function `aph-keys--update-major-mode'.")

(defvar aph-keys-mode-map (make-sparse-keymap)
  "Global keymap for `aph-keys-mode'.
To bind a key in `aph-keys-mode' conditionally on a major or
minor mode, use `aph-keys-augment'.")

(add-to-list 'emulation-mode-map-alists
             'aph-keys-minor-mode-map-alist :append #'eq)
(add-to-list 'emulation-mode-map-alists
             'aph-keys-local-map-alist :append #'eq)

(define-minor-mode aph-keys-mode
  "Mode for the personal keybindings of Aaron Harris."
  :global  t
  :lighter " #"
  ;; Control of whether augmented minor mode maps are active occurs
  ;; here.  For major modes, see `aph-keys--update-major-mode'.
  (setq aph-keys-minor-mode-map-alist
        (if aph-keys-mode aph-keys-augment-map-alist nil)))


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

(defun aph-keys-augment--register (mode)
  "Register augmented keymap for MODE.
Add the augmented keymap for MODE (a symbol) to
`aph-keys-augment-map-alist'."
  (push (cons mode (aph-keys-augment mode))
        aph-keys-augment-map-alist))

(defun aph-keys-augment-var (mode)
  "As `aph-keys-augment', but return a variable.
This variable contains the keymap that would be returned by
`aph-keys-augment'."
  (unless (aph-keys-augmented-p mode) 
      (eval `(aph-keys-augment--define ,mode))
      (aph-keys-augment--register mode))
  (aph-keys--augment-name mode))

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


;;; `aph-keys-mode': Major mode support
;;;====================================
(defun aph-keys--update-major-mode ()
  "Update `aph-keys-local-mode-map-alist' for current major mode.

Update the keymap stored in the cdr of
`aph-keys-local-mode-map-alist' so that it reflects the current
major mode.  If the current major mode is augmented, this keymap
should be that augmented keymap, inheriting from
`aph-keys-mode-map'.  Otherwise, it should just be
`aph-keys-mode-map'.  See `aph-keys-augment' for more information
on augmented keymaps.

Also delete any entry keyed off the current ajor mode from
`aph-keys-minor-map-alist', as this can cause augmented bindings
for the major mode to inappropriately shadow bindings for
augmented minor modes.

This function is suitable for use in
`after-change-major-mode-hook'."
  (let ((keymap
         (if (aph-keys-augmented-p major-mode)
             (let ((augmap (aph-keys-augment major-mode)))
               (unless (keymap-parent augmap)
                 (set-keymap-parent augmap aph-keys-mode-map))
               augmap)
           aph-keys-mode-map)))
    (setq aph-keys-minor-mode-map-alist
          (assq-delete-all major-mode aph-keys-minor-mode-map-alist))
    (setq aph-keys-local-map-alist `((aph-keys-mode . ,keymap)))))

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
