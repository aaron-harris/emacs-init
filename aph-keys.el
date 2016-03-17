;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; PERSONAL KEYBINDING MODE
;;;;============================================================================

;; A minor mode for implementing my personal keybindings, along with
;; some other utilities related to keybindings.

(require 'aph-symbol)                   ; For `aph/symbol-concat'
(require 'aph-bind-key)                 ; For `bind-keys' and fixes
(require 'cl-lib)                       ; For `cl-defun'


;;; `aph-keys-mode': Keymap Setup
;;;==============================
(defvar aph-keys-augment-map-alist nil
  "Alist of augmented keymaps for `aph-keys-mode'.
This alist contains all keymaps augmented for use by
`aph-keys-mode'.  See the function `aph-keys-augment' for more
information.")

(defvar-local aph-keys-overriding-map-alist
  `((aph-keys-mode . ,(make-sparse-keymap)))
  "An alist holding the overriding keymap for current major mode.

This variable uses the same format as `aph-keys-local-map-alist',
but takes precedence over both the keymap stored there and all
the keymaps in `aph-keys-minor-mode-map-alist'.

Like `aph-keys-local-map-alist', the association between this
variable and the major mode is maintained by the function
`aph-keys--update-major-mode'.")

(defvar aph-keys-minor-mode-map-alist nil
  "Alist of active minor mode keymaps for `aph-keys-mode'.
For use in `emulation-mode-map-alists'.

Note that augmented keymaps for major modes may appear in this
list, but their presence does nothing since there is no
corresponding control variable for a major mode.  See
`aph-keys-local-map-alist' and `aph-keys--update-major-mode' for
the mechanism that manages the augmented keymap for the current
major mode.

The value of this variable is managed by `aph-keys-mode', and
changes made directly to it are liable to be overwritten.  The
variable `aph-keys-augment-map-alist' usually holds the same
data, but is more permanent.  But generally speaking, even
modifying that variable directly should not be necessary.")

(defvar-local aph-keys-local-map-alist
  `((aph-keys-mode . ,(make-sparse-keymap)))
  "An alist holding the augmented keymap for current major mode.

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

This keymap is the equivalent of `global-map' for
`aph-keys-mode', and its bindings will be active whenever
`aph-keys-mode' is enabled, provided they are not shadowed by a
higher-priority keymap, such as an augmented keymap obtained
from `aph-keys-augment'.")

(defvar aph-keys-mode-overriding-map (make-sparse-keymap)
  "Overriding keymap for `aph-keys-mode'.

The bindings in this keymap are active only with `aph-keys-mode'
and take precedence over all other bindings made by
`aph-keys-mode', save those in the overriding augmented keymap
for the current major mode (see `aph-keys-augment').")

(defvar aph-keys-mode-minibuffer-map
  (let ((k (make-sparse-keymap)))
    (set-keymap-parent k aph-keys-mode-map)
    k)
  "Minibuffer keymap for `aph-keys-mode'.

This keymap is used in place of the augmented keymap for the
current major mode inside a minibuffer.  Bind keys here if you
wish them to override keys in any of the `minibuffer-local-*-map'
keymaps.  Note that this keymap is used regardless of which of
those keymaps is currently in use.")

(defvar aph-keys-mode-helm-map
  (let ((k (make-sparse-keymap)))
    (set-keymap-parent k aph-keys-mode-minibuffer-map)
    k)
  "Keymap for use with `helm' in `aph-keys-mode'.
Inherits from `aph-keys-mode-minibuffer-map'

This keymap is used in place of the augmented keymap for the
current major mode during a `helm' session.  Bind keys here if
you wish them to override keys in `helm-map'.")

(add-to-list 'emulation-mode-map-alists
             'aph-keys-overriding-map-alist :append #'eq)
(add-to-list 'emulation-mode-map-alists
             'aph-keys-minor-mode-map-alist :append #'eq)
(add-to-list 'emulation-mode-map-alists
             'aph-keys-local-map-alist :append #'eq)

(define-minor-mode aph-keys-mode
  "Mode for the personal keybindings of Aaron Harris.

To bind a key globally (in a way that shadows, rather than
overwrites, the default binding), use `aph-keys-mode-map'.  To
bind a key in the same way, but specific to a particular major or
minor mode, pass the symbol naming the mode to the function
`aph-keys-augment', and then bind the key in the resulting
keymap."
  :global  t
  :lighter " #" 
  ;; Make sure the appropriate major and minor mode maps are active.
  (setq aph-keys-minor-mode-map-alist
        (if aph-keys-mode aph-keys-augment-map-alist nil))
  (when aph-keys-mode (aph-keys--update-major-mode)))


;;; `aph-keys-mode': Augmented keymaps
;;;=================================== 
(defun aph-keys-augment-name (mode &optional override)
  "Return the name of the augmented keymap for MODE.
Do not augment MODE if it is not already.

If OVERRIDE is non-nil, return the name of the overriding augment
map instead.

The returned symbol is the name of the variable that would
contain the augmented keymap for MODE, if one exists.  See
`aph-keys-augment' for more information."
  (aph/symbol-concat 'aph-keys (format "%s-mode-map:%s"
                                       (if override "-overriding" "")
                                       mode)))

(defun aph-keys-augmented-p (mode &optional override)
  "Return non-nil if MODE has an augmented keymap.

If OVERRIDE is non-nil, consider the overriding augment map
instead of the ordinary one.

See `aph-keys-augment' for more information."
  (let ((augmap (aph-keys-augment-name mode override)))
    (boundp augmap)))

(defmacro aph-keys-augment--define (mode &optional override)
  "Define augmented keymap variable for MODE.

If OVERRIDE is non-nil, define the overriding augment map
instead.

Used by `aph-keys-augment'.  See that function for more
details."
  (declare (debug (symbolp)))
  (let ((augmap (aph-keys-augment-name mode override)))
    `(defvar ,augmap (make-sparse-keymap)
       ,(format (concat "%s keymap for `%s'.\n"
                        "See `aph-keys-augment' for more details.")
                (if override "Overriding augmented" "Augmented")
                mode))))

(defun aph-keys-augment--register (mode)
  "Register augmented keymap for MODE.
Add the augmented keymap for MODE (a symbol) to
`aph-keys-augment-map-alist'."
  (push (cons mode (aph-keys-augment mode))
        aph-keys-augment-map-alist)
  ;; Also update `aph-keys-minor-mode-map-alist' so new keymap gets
  ;; picked up immediately.
  (when aph-keys-mode (setq aph-keys-minor-mode-map-alist
                            aph-keys-augment-map-alist)))

(defun aph-keys-augment-var (mode &optional override)
  "As `aph-keys-augment-name', but augment MODE."
  (unless (aph-keys-augmented-p mode override)
      (eval `(aph-keys-augment--define ,mode ,override))
      (unless override (aph-keys-augment--register mode)))
  (aph-keys-augment-name mode override))

(defun aph-keys-augment (mode &optional override)
  "Return augmented keymap corresponding to MODE for `aph-keys-mode'.

The parameter MODE should be a symbol naming a major or minor
mode (e.g., the symbol 'text-mode).  Other symbols can be passed,
but this may or may not work as expected.

The keymap returned will be active whenever both MODE (or a mode
descended from MODE) and `aph-keys-mode' are active.  This
provides a mechanism for mode-local keybindings that are still
toggleable with `aph-keys-mode'.

If OVERRIDE is non-nil, then MODE should be a major mode, and the
keymap returned takes precedence over all ordinary augmented
maps.

Subsequent calls to this function with the same arguments return
the same keymap, including any bindings that were made to that
keymap after its construction.  That is, there is at most one
augmented keymap and one overriding keymap for each mode, and
this function returns the appropriate such keymap, constructing
it if necessary.

The variable holding a non-overriding augmented keymap is named
using the format 'aph-keys-mode-map:MODE',
e.g. 'aph-keys-mode-map:text-mode'.  Overriding keymaps are named
using the format 'aph-keys-overriding-mode-map:MODE."
  (symbol-value (aph-keys-augment-var mode override)))


;;; `aph-keys-mode': Major mode support
;;;====================================
(defun aph-keys--set-major-mode-parentage (mode &optional override)
  "Set parentage of augmented keymap for MODE.

Set the parentage of the augmented keymap for MODE to reflect the
parentage of MODE as a derived major mode, augmenting ancestral
modes as necessary.  The augmented keymap for a non-derived mode
inherits from `aph-keys-mode-map'.  Return the modified keymap.

For example, suppose that MODE is a mode foo-mode that derives
from bar-mode, and bar-mode in turn derives from `text-mode'.
Then
* First, bar-mode and `text-mode' would be augmented (if they weren't
  already).
* The parent for the keymap (aph-keys-augment foo-mode) would be set
  to (aph-keys-augment bar-mode).
* The parent for (aph-keys-augment bar-mode) would be
  (aph-keys-augment text-mode).
* The parent for (aph-keys-augment text-mode) would be
  `aph-keys-mode-map'.
* Finally, (aph-keys-augment foo-mode) would be returned.

If the OVERRIDE parameter is non-nil, the keymap acted upon is
the overriding keymap for MODE.  Its parentage is similar, but
all of the keymaps in the chain are overriding keymaps.

Do not pass minor modes to this function, as it will likely
disrupt precedence of augmented keymaps."
  (let ((keymap  (aph-keys-augment mode override))
        (parent  (get mode 'derived-mode-parent)))
    (set-keymap-parent
     keymap
     (cond
      (parent    (aph-keys--set-major-mode-parentage parent override))
      (override  aph-keys-mode-overriding-map)
      (:else     aph-keys-mode-map)))
    keymap))

(defun aph-keys--update-major-mode ()
  "Update `aph-keys-local-mode-map-alist' for current major mode.

Update the keymap stored in the cddr of
`aph-keys-local-mode-map-alist' so that it reflects the current
major mode.  This will call `aph-keys--set-major-mode-parentage'
to update the parentage of the augmented map for that mode.

This function is suitable for use in
`after-change-major-mode-hook'."
  (let* ((helm-p  (and (fboundp #'helm-alive-p) (helm-alive-p)))
         (local-map
          (cond
           (helm-p         aph-keys-mode-helm-map)
           ((minibufferp)  aph-keys-mode-minibuffer-map)
           (:else  (aph-keys--set-major-mode-parentage major-mode))))
         (override-map
          (cond
           (helm-p         aph-keys-mode-overriding-map)
           ((minibufferp)  aph-keys-mode-overriding-map)
           (:else  (aph-keys--set-major-mode-parentage major-mode :override))))) 
    (setq aph-keys-local-map-alist      `((aph-keys-mode . ,local-map))
          aph-keys-overriding-map-alist `((aph-keys-mode . ,override-map)))))

(add-hook 'after-change-major-mode-hook #'aph-keys--update-major-mode)


;;; `bind-keys' Support
;;;====================
(defun aph/bind-keys-augment-advice (orig &rest args)
  "Advice to add :augment, :override keywords to `bind-keys'.

The :augment keyword takes the name of a major or minor mode and
binds the specified keys in the augmented keymap associated with
that mode, for use with `aph-keys-mode'.  See `aph-keys-augment'
for more information.

The :override keyword behaves similarly, but uses the overriding
augment keymap.

Both keywords accept lists, just like :map, and are compatible
with :map; for example,

    (bind-keys :augment foo-mode
               :map (bar-mode-map
                     baz-mode-map) ...)

is more or less equivalent to

    (bind-keys :map (aph-keys-mode-map:foo-mode
                     bar-mode-map
                     baz-mode-map) ...).

Intended as :around advice for `bind-keys'."
  (require 'aph-plist)                  ; For `aph/plist-get-as-list'
  (let* ((augment   (aph/plist-get-as-list args :augment))
         (override  (aph/plist-get-as-list args :override))
         (maps      (aph/plist-get-as-list args :map)))
    ;; Note that we don't remove our keys.  Instead we're relying on
    ;; undocumented behavior of `bind-keys', specifically that it
    ;; simply ignores keyword arguments it does not recognize (as well
    ;; as any instances of a particular keyword after the first).
    `(progn ,@(cl-loop for mode in augment
                       do (push (aph-keys-augment-name mode) maps)
                       collect `(aph-keys-augment ',mode))
            ,@(cl-loop for mode in override
                       do (push (aph-keys-augment-name mode :override) maps)
                       collect `(aph-keys-augment ',mode :override))
            ,(apply orig `(:map ,maps ,@args)))))

(advice-add #'bind-keys :around #'aph/bind-keys-augment-advice)


;;; Compatibility Functions
;;;========================
(defun aph-keys-default-command (key &optional default)
  "Execute the command bound to KEY without `aph-keys-mode'.

Execute whatever command would be bound to KEY if `aph-keys-mode'
were not active.  If DEFAULT is provided, use that instead if no
command would normally be bound to KEY; otherwise, call
`undefined'.

This function serves as the back end for the commands
`aph-keys-default-return-command' and
`aph-keys-default-tab-command'." 
  (let ((aph-keys-mode                  nil)
        (aph-keys-minor-mode-map-alist  nil))
    ;; Do everything possible to let the command called think it was
    ;; called via its usual binding, as doing otherwise can confuse
    ;; some commands.
    (let* ((char               (string-to-char key))
           (last-command-event char))
      (call-interactively (or (key-binding key) default #'undefined)
                          (not :record-flag)
                          (vector (string-to-char key))))))

(defun aph-keys-default-return-command ()
  "Execute the command bound to RET without `aph-keys-mode'.

Since `aph-keys-mode' shadows ordinary (non-augmented)
mode-specific bindings, it can be difficult to bind
C-m (a.k.a. RET) in `aph-keys-mode' separately from <return>.
This command attempts to alleviate that difficulty by executing
whatever command would be bound to RET if `aph-keys-mode' were
not active.

If you intend to bind C-m separately from <return> in
`aph-keys-mode-map' or any augmented keymap, bind this command to
<return> in `aph-keys-mode-map'."
  (interactive)
  (aph-keys-default-command (kbd "RET") #'newline))

(defun aph-keys-default-tab-command ()
  "Execute the command bound to TAB without `aph-keys-mode'.

Since `aph-keys-mode' shadows ordinary (non-augmented)
mode-specific bindings, it can be difficult to bind
C-i (a.k.a. TAB) in `aph-keys-mode' separately from <tab>.
This command attempts to alleviate that difficulty by executing
whatever command would be bound to TAB if `aph-keys-mode' were
not active.

If you intend to bind C-i separately from <tab> in
`aph-keys-mode-map' or any augmented keymap, bind this command to
<tab> in `aph-keys-mode-map'."
  (interactive)
  (aph-keys-default-command (kbd "TAB") #'indent-for-tab-command))


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


;;; Conditional Keybindings
;;;========================
;; Code in this section aims to establish conditional keybindings and
;; is adapted from a macro found here:
;;   http://endlessparentheses.com/define-context-aware-keys-in-emacs.html

(defun aph/keys--construct-conditional-binding (def condition)
  "Return a menu item defining a binding for DEF subject to CONDITION.

The returned menu item can be used with `define-key' to define a
binding for COMMAND that is made transparent whenever
CONDITION (a single form) evaluates to nil."
  `(menu-item
    ,(format "maybe-%s" (or (car (cdr-safe def)) def))
    nil
    :filter (lambda (&optional _)
              (when ,condition ,def))))

(defmacro aph/keys-define-conditionally (keymap key def
                                                 &rest body)
  "In KEYMAP, define key sequence KEY as DEF conditionally.
This is like `define-key', except the definition
\"disappears\" whenever BODY evaluates to nil."
  (declare (indent 3)
           (debug (form form form &rest sexp)))
  `(define-key ,keymap ,key
     ',(aph/keys--construct-conditional-binding def `(progn ,@body))))


(provide 'aph-keys)
