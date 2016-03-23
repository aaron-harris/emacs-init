;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; UMBRA MODE
;;;;============================================================================

;; A minor mode for implementing keybindings in a way that shadows,
;; rather than overriding, existing keybindings.

(require 'aph-symbol)                   ; For `aph/symbol-concat'
(require 'aph-bind-key)                 ; For `bind-keys' and fixes
(require 'cl-lib)                       ; For `cl-defun'


;;; Keymap Setup
;;;=============
(defvar umbra-map-alist nil
  "Alist containing all augmented (\"umbra\") keymaps.

This alist contains all keymaps augmented for use by
`umbra-mode'.  See the function `umbra-augment' for more
information.

Overriding (\"penumbra\") keymaps are not included.")

(defvar-local umbra-overriding-map-alist
  `((umbra-mode . ,(make-sparse-keymap)))
  "Alist holding the current overriding (\"penumbra\") keymap.
This keymap is the penumbra map for the current major mode.

This variable uses the same format as `umbra-local-map-alist',
but takes precedence over both the keymap stored there and all
the keymaps in `umbra-minor-mode-map-alist'.

Like `umbra-local-map-alist', the association between this
variable and the major mode is maintained by the function
`umbra--update-major-mode'.")

(defvar umbra-minor-mode-map-alist nil
  "Alist of umbra keymaps for minor modes.
For use in `emulation-mode-map-alists'.

Note that umbra keymaps for major modes may appear in this list,
but their presence does nothing since there is no corresponding
control variable for a major mode.  See `umbra-local-map-alist'
and `umbra--update-major-mode' for the mechanism that manages the
umbra keymap for the current major mode.

The value of this variable is managed by `umbra-mode', and
changes made directly to it are liable to be overwritten.  The
variable `umbra-map-alist' usually holds the same data, but is
more permanent.  But generally speaking, even modifying that
variable directly should not be necessary.")

(defvar-local umbra-local-map-alist
  `((umbra-mode . ,(make-sparse-keymap)))
  "An alist holding the umbra keymap for current major mode.

This alist should contain a single element of the
form ('umbra-mode . KEYMAP), where KEYMAP is the umbra keymap
corresponding to the current major mode.  See `umbra-keymap' for
more information about augmented keymaps.

This variable is intended for use in `emulation-mode-map-alists'.
It is separate from `umbra-map-alist' so that it can be
buffer-local and thus vary with the buffer's major mode.

The association between this variable and the major mode is
maintained by the function `umbra--update-major-mode'.")

(defvar umbra-mode-map (make-sparse-keymap)
  "Global keymap for `umbra-mode'.

This keymap is the equivalent of `global-map' for `umbra-mode',
and its bindings will be active whenever `umbra-mode' is enabled,
provided they are not shadowed by an umbra or penumbra keymap.")

(defvar umbra-mode-overriding-map (make-sparse-keymap)
  "Overriding keymap for `umbra-mode'.

The bindings in this keymap are active only with `umbra-mode' and
take precedence over all other bindings made by `umbra-mode',
save those in the penumbra keymap for the current major mode (see
`umbra-keymap').")

(defvar umbra-mode-minibuffer-map
  (let ((k (make-sparse-keymap)))
    (set-keymap-parent k umbra-mode-map)
    k)
  "Minibuffer keymap for `umbra-mode'.

This keymap is used in place of the umbra keymap for the current
major mode inside a minibuffer.  Bind keys here if you wish them
to override keys in any of the `minibuffer-local-*-map' keymaps.
Note that this keymap is used regardless of which of those
keymaps is currently in use.")

(defvar umbra-mode-helm-map
  (let ((k (make-sparse-keymap)))
    (set-keymap-parent k umbra-mode-minibuffer-map)
    k)
  "Keymap for use with `helm' in `umbra-mode'.
Inherits from `umbra-mode-minibuffer-map'

This keymap is used in place of the umbra keymap for the current
major mode during a `helm' session.  Bind keys here if you wish
them to override keys in `helm-map'.")

(add-to-list 'emulation-mode-map-alists
             'umbra-overriding-map-alist :append #'eq)
(add-to-list 'emulation-mode-map-alists
             'umbra-minor-mode-map-alist :append #'eq)
(add-to-list 'emulation-mode-map-alists
             'umbra-local-map-alist :append #'eq)

(define-minor-mode umbra-mode
  "Mode for reversible keybindings.

To bind a key globally (in a way that shadows, rather than
overwrites, the default binding), use `umbra-mode-map'.  To bind
a key in the same way, but specific to a particular major or
minor mode, pass the symbol naming the mode to the function
`umbra-keymap', and then bind the key in the resulting keymap.

If you with to use the `bind-key' package with `umbra-mode', two
additional keywords are provided for `bind-keys', :umbra
and :penumbra.

The :umbra keyword is similar to the existing :map keyword; it
takes the symbol naming a mode, or a list of such symbols, and
makes all bindings in the umbra map associated with that mode.

The :penumbra keyword is analogous, but makes its bindings in the
penumbra map instead."
  :global t
  :lighter " #"
  (setq umbra-minor-mode-map-alist
        (if umbra-mode umbra-map-alist nil))
  (when umbra-mode (umbra--update-major-mode)))


;;; Augmented keymaps
;;;==================
(defun umbra-keymap-name (mode &optional penumbra)
  "Return the name of the umbra keymap for MODE.
Do not create such a keymap if it does not already exist.

If PENUMBRA is non-nil, return the name of the penumbra map
instead.

The returned symbol is the name of the variable that would
contain the umbra keymap for MODE, if one exists.  See
`umbra-keymap' for more information."
  (aph/symbol-concat 'umbra (format "%s-mode-map:%s"
                                    (if penumbra "-overriding" "")
                                    mode)))

(defun umbra-has-keymap-p (mode &optional penumbra)
  "Return non-nil if MODE has an umbra keymap.
If PENUMBRA is non-nil, consider the penumbra map instead.

See `umbra-keymap' for more information."
  (let ((augmap (umbra-keymap-name mode penumbra)))
    (boundp augmap)))

(defmacro umbra-keymap--define (mode &optional penumbra)
  "Define umbra keymap variable for MODE.

If PENUMBRA is non-nil, define the penumbra keymap instead.

Used by `umbra-keymap'.  See that function for more details."
  (declare (debug (symbolp)))
  (let ((augmap (umbra-keymap-name mode penumbra)))
    `(defvar ,augmap (make-sparse-keymap)
       ,(format (concat "%s keymap for `%s'.\n"
                        "See `umbra-keymap' for more details.")
                (if penumbra "Overriding augmented" "Augmented")
                mode))))

(defun umbra-keymap--register (mode)
  "Add the umbra keymap for MODE (a symbol) to `umbra-map-alist'."
  (push `(,mode . ,(umbra-keymap mode)) umbra-map-alist)
  ;; Also update `umbra-minor-mode-map-alist' so new keymap gets
  ;; picked up immediately.
  (when umbra-mode (setq umbra-minor-mode-map-alist umbra-map-alist)))

(defun umbra-keymap-var (mode &optional penumbra)
  "As `umbra-keymap-name', but create keymap if necessary."
  (unless (umbra-has-keymap-p mode penumbra)
    (eval `(umbra-keymap--define ,mode ,penumbra))
    (unless penumbra (umbra-keymap--register mode)))
  (umbra-keymap-name mode penumbra))

(defun umbra-keymap (mode &optional penumbra)
  "Return umbra keymap corresponding to MODE for `umbra-mode'.

The parameter MODE should be a symbol naming a major or minor
mode (e.g., the symbol 'text-mode).  Other symbols can be passed,
but this may or may not work as expected.

The keymap returned will be active whenever both MODE (or a mode
descended from MODE) and `umbra-mode' are active.  This provides
a mechanism for mode-local keybindings that are still toggleable
with `umbra-mode'.

If PENUMBRA is non-nil, then MODE should be a major mode, and the
keymap returned (the \"penumbra keymap\" for MODE) takes
precedence over all ordinary augmented maps.

Subsequent calls to this function with the same arguments return
the same keymap, including any bindings that were made to that
keymap after its construction.  That is, there is at most one
umbra keymap and one penumbra keymap for each mode, and this
function returns the appropriate such keymap, constructing it if
necessary.

The variable holding an umbra keymap is named using the format
'umbra-mode-map:MODE', e.g. 'umbra-mode-map:text-mode'.  Penumbra
keymaps are named like 'umbra-overriding-mode-map:MODE."
  (symbol-value (umbra-keymap-var mode penumbra)))


;;; Major mode support
;;;===================
(defun umbra--set-major-mode-parentage (mode &optional penumbra)
  "Set parentage of umbra keymap for MODE.

Set the parentage of the umbra keymap for MODE to reflect the
parentage of MODE as a derived major mode, augmenting ancestral
modes as necessary.  The umbra keymap for a non-derived mode
inherits from `umbra-mode-map'.  Return the modified keymap.

For example, suppose that MODE is a mode foo-mode that derives from
bar-mode, and bar-mode in turn derives from `text-mode'.  Then:

* First, the umbra maps for bar-mode and `text-mode' are created,
  if they don't already exist.
* The parent for the keymap (umbra-keymap foo-mode) is set
  to (umbra-keymap bar-mode).
* The parent for (umbra-keymap bar-mode) is (umbra-keymap text-mode).
* The parent for (umbra-keymap text-mode) is `umbra-mode-map'.
* Finally, (umbra-keymap foo-mode) is returned.

If the PENUMBRA parameter is non-nil, the keymap acted upon is
the penumbra keymap for MODE.  Its parentage is similar, but all
of the keymaps in the chain are penumbra maps.

Do not pass minor modes to this function, as it will likely
disrupt precedence of `umbra-mode' keymaps."
  (let ((keymap  (umbra-keymap mode penumbra))
        (parent  (get mode 'derived-mode-parent)))
    (set-keymap-parent
     keymap
     (cond
      (parent    (umbra--set-major-mode-parentage parent penumbra))
      (penumbra  umbra-mode-overriding-map)
      (:else     umbra-mode-map)))
    keymap))

(defun umbra--update-major-mode ()
  "Update `umbra-local-mode-map-alist' for current major mode.

Update the keymap stored in the cddr of `umbra-local-mode-map-alist'
so that it reflects the current major mode.  This will call
`umbra--set-major-mode-parentage' to update the parentage of the umbra
map for that mode.

This function is suitable for use in `after-change-major-mode-hook'."
  (let* ((helm-p  (and (fboundp #'helm-alive-p) (helm-alive-p)))
         (umbra-map
          (cond
           (helm-p         umbra-mode-helm-map)
           ((minibufferp)  umbra-mode-minibuffer-map)
           (:else  (umbra--set-major-mode-parentage major-mode))))
         (penumbra-map
          (cond
           (helm-p         umbra-mode-overriding-map)
           ((minibufferp)  umbra-mode-overriding-map)
           (:else  (umbra--set-major-mode-parentage major-mode :penumbra)))))
    (setq umbra-local-map-alist      `((umbra-mode . ,umbra-map))
          umbra-overriding-map-alist `((umbra-mode . ,penumbra-map)))))

(add-hook 'after-change-major-mode-hook #'umbra--update-major-mode)


;;; `bind-keys' Support
;;;====================
(defun umbra--bind-keys-advice (orig &rest args)
  "Advice to add :umbra, :penumbra keywords to `bind-keys'.

The :umbra keyword takes the name of a major or minor mode and
binds the specified keys in the umbra keymap associated with that
mode, for use with `umbra-mode'.  See `umbra-keymap' for more
information.

The :penumbra keyword behaves similarly, but uses the penumbra keymap.

Both keywords accept lists, just like :map, and are compatible
with :map; for example,

    (bind-keys :umbra foo-mode
               :map (bar-mode-map
                     baz-mode-map) ...)

is more or less equivalent to

    (bind-keys :map (umbra-mode-map:foo-mode
                     bar-mode-map
                     baz-mode-map) ...).

Intended as :around advice for `bind-keys'."
  (require 'aph-plist)                  ; For `aph/plist-get-as-list'
  (let* ((umbra     (aph/plist-get-as-list args :umbra))
         (penumbra  (aph/plist-get-as-list args :penumbra))
         (maps      (aph/plist-get-as-list args :map)))
    ;; Note that we don't remove our keys.  Instead we're relying on
    ;; undocumented behavior of `bind-keys', specifically that it
    ;; simply ignores keyword arguments it does not recognize (as well
    ;; as any instances of a particular keyword after the first).
    `(progn ,@(cl-loop for mode in umbra
                       do (push (umbra-keymap-name mode) maps)
                       collect `(umbra-keymap ',mode))
            ,@(cl-loop for mode in penumbra
                       do (push (umbra-keymap-name mode :penumbra) maps)
                       collect `(umbra-keymap ',mode :penumbra))
            ,(apply orig `(:map ,maps ,@args)))))

(advice-add #'bind-keys :around #'umbra--bind-keys-advice)


;;; Compatibility Functions
;;;========================
(defun umbra-default-command (key &optional default)
  "Execute the command bound to KEY without `umbra-mode'.

Execute whatever command would be bound to KEY if `umbra-mode'
were not active.  If DEFAULT is provided, use that instead if no
command would normally be bound to KEY; otherwise, call `undefined'.

This function serves as the back end for the commands
`umbra-default-return-command' and `umbra-default-tab-command'."
  (let ((umbra-mode                  nil)
        (umbra-minor-mode-map-alist  nil))
    ;; Do everything possible to let the command called think it was
    ;; called via its usual binding, as doing otherwise can confuse
    ;; some commands.
    (let* ((char                (string-to-char key))
           (last-command-event  char))
      (call-interactively (or (key-binding key) default #'undefined)
                          (not :record-flag)
                          (vector (string-to-char key))))))

(defun umbra-default-return-command ()
  "Execute the command bound to RET without `umbra-mode'.

Since `umbra-mode' shadows ordinary mode-specific bindings, it
can be difficult to bind C-m (a.k.a. RET) in `umbra-mode'
separately from <return>.  This command attempts to alleviate
that difficulty by executing whatever command would be bound to
RET if `umbra-mode' were not active.

If you intend to bind C-m separately from <return> in
`umbra-mode-map' or any augmented keymap, bind this command to
<return> in `umbra-mode-map'."
  (interactive)
  (umbra-default-command (kbd "RET") #'newline))

(defun umbra-default-tab-command ()
  "Execute the command bound to TAB without `umbra-mode'.

Since `umbra-mode' shadows ordinary mode-specific bindings, it
can be difficult to bind C-i (a.k.a. TAB) in `umbra-mode'
separately from <tab>.  This command attempts to alleviate that
difficulty by executing whatever command would be bound to TAB if
`umbra-mode' were not active.

If you intend to bind C-i separately from <tab> in
`umbra-mode-map' or any augmented keymap, bind this command to
<tab> in `umbra-mode-map'."
  (interactive)
  (umbra-default-command (kbd "TAB") #'indent-for-tab-command))


(provide 'umbra)
