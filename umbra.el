;;; umbra.el --- Minor mode for non-overriding keys -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience keybinding

;; Dependencies: `aph-symbol', `aph-plist' (only with `bind-key')
;; Advised functions from other packages:
;;   bind-key: `bind-keys'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module defines the minor mode `umbra-mode'.  This mode is
;; designed to hold all of your personal keybindings, including
;; mode-specific bindings.  Making your bindings in this way makes it
;; easy to revert changes or temporarily disable all of your custom
;; keybindings (e.g., to make use of a default binding you have
;; inadvertently shadowed, or just to check what the default binding
;; for a key is).
;;
;; To accomplish this, `umbra-mode' reimplements much of the standard
;; keymap hierarchy on top of the existing heirarchy (within
;; `emulation-mode-map-alists', specifically).
;;
;; To use `umbra-mode', just enable it in the usual way and add
;; keybindings to one of these keymaps:
;;
;; * The keymap `umbra-mode-map' is the equivalent of `global-map';
;; bindings made here will be active whenever `umbra-mode' is enabled
;; and are shadowed by bindings in all other `umbra-mode' keymaps.
;;
;; * Each major and minor mode can be equipped with a so-called
;; "umbra" map.  This is the equivalent of that mode's ordinary map
;; within the `umbra-mode' heirarchy.  The usual keymap precedence
;; rule is maintained: minor modes' umbra maps take precedence over
;; the major mode's umbra map, which takes precedence over
;; `umbra-mode-map'.
;;
;; To equip a mode with an umbra map, use the function `umbra-keymap'.
;; This function takes a mode and returns the umbra keymap for that
;; mode, creating it if necessary.  Then just make your keybindings in
;; that keymap.
;;
;; * The keymap `umbra-mode-overriding-map' takes precedence over all
;; umbra maps.  This is rarely used, but can be useful if you use
;; conditional keybindings (i.e., menu items with :filter functions).
;;
;; * Each major mode can additionally be equipped with a "penumbra"
;; map.  This is the equivalent of `overriding-local-map' and takes
;; precedence over all umbra maps and over `umbra-mode-overriding-map'.
;;
;; To get the penumbra map for a major mode, pass a non-nil second
;; parameter to `umbra-keymap' (e.g., (umbra-keymap 'org-mode t)).
;;
;; * Finally, there is `umbra-mode-minibuffer-map'.  This keymap is
;; used in place of the major mode's umbra map during minibuffer
;; input, and as such is the equivalent of the various
;; `minibuffer-local-*-map' keymaps.
;;
;; If you use `helm', you can also use the keymap
;; `umbra-mode-helm-map'.  This is the equivalent of `helm-map'; it
;; inherits from `umbra-mode-minibuffer-map' and is used only inside a
;; `helm' session.
;;
;;
;; Finally, support is provided for the `bind-key' package in the form
;; of two new keywords for the `bind-keys' macro, :umbra and
;; :penumbra.  These are similar to the :map keyword, but take mode
;; names rather than keymaps.  For example, the form
;;
;;    (bind-keys :umbra foo-mode
;;               :penumbra (bar-mode baz-mode)
;;               :map some-other-keymap
;;               ("C-f" . frobnicate))
;;
;; binds the frobnicate command to `C-f' in the umbra map for
;; foo-mode, the penumbra maps for bar-mode and baz-mode, and in
;; some-other-keymap.

;;; Code:

(require 'aph-symbol)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'aph-subr))


;;;; User Options
;;;;=============
(defgroup umbra nil
  "Shadow keybindings without overwriting."
  :prefix "umbra-"
  :link '(emacs-commentary-link "umbra")
  :group 'convenience)


;;;; Base Keymaps
;;;;=============
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


;;;; Keymap Management
;;;;==================
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

(add-to-list 'emulation-mode-map-alists
             'umbra-overriding-map-alist :append #'eq)
(add-to-list 'emulation-mode-map-alists
             'umbra-minor-mode-map-alist :append #'eq)
(add-to-list 'emulation-mode-map-alists
             'umbra-local-map-alist :append #'eq)


;;;; Augmented keymaps
;;;;==================
(defmacro define-umbra-keymap (name mode &optional penumbra)
  "Define umbra keymap variable with NAME for MODE.

If PENUMBRA is non-nil, define the penumbra keymap instead.

Note that NAME can be easily constructed from MODE using
`umbra-keymap-name', but this is left to the caller so that this
macro can meet the coding conventions detailed in the info
node `(elisp) Coding Conventions'.

Used by `umbra-keymap'.  See that function for more details."
  (declare (debug (symbolp symbolp form)))
  `(defvar ,name (make-sparse-keymap)
     ,(format (concat "%s keymap for `%s'.\n"
                      "See `umbra-keymap' for more details.")
              (if penumbra "Overriding augmented" "Augmented")
              mode)))

(defun umbra-keymap--register (mode)
  "Add the umbra keymap for MODE (a symbol) to `umbra-map-alist'."
  (push `(,mode . ,(umbra-keymap mode)) umbra-map-alist)
  ;; Also update `umbra-minor-mode-map-alist' so new keymap gets
  ;; picked up immediately.
  (when umbra-mode (setq umbra-minor-mode-map-alist umbra-map-alist)))

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
  (boundp (umbra-keymap-name mode penumbra)))

(defun umbra-keymap-var (mode &optional penumbra)
  "As `umbra-keymap-name', but create keymap if necessary."
  (unless (umbra-has-keymap-p mode penumbra)
    (eval `(define-umbra-keymap ,(umbra-keymap-name mode penumbra)
             ,mode ,penumbra))
    (unless penumbra (umbra-keymap--register mode)))
  (umbra-keymap-name mode penumbra))

;;;###autoload
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


;;;; Mode Definition
;;;;================ 
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

;;;###autoload
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
  (setq umbra-minor-mode-map-alist (if umbra-mode umbra-map-alist nil))
  (when umbra-mode (umbra--update-major-mode)))

(add-hook 'after-change-major-mode-hook #'umbra--update-major-mode)


;;;; `bind-keys' Support
;;;;====================
(defun umbra--plist-get-as-list (plist prop)
  "As `plist-get', but ensure result is a list.
If `plist-get' would return a list, return that list.  Otherwise,
return a list containing that value as its sole element."
  (let ((val (plist-get plist prop)))
    (if (listp val)
        val
      (list val)))) 

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
  (let* ((umbra     (umbra--plist-get-as-list args :umbra))
         (penumbra  (umbra--plist-get-as-list args :penumbra))
         (maps      (umbra--plist-get-as-list args :map)))
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


;;;; Compatibility Functions
;;;;========================
(defun umbra-default-binding (key)
  "Return the command bound to KEY without `umbra-mode'."
  (let ((umbra-mode                  nil)
        (umbra-minor-mode-map-alist  nil))
    (key-binding key)))

(defun umbra-default-command (&rest keys)
  "Execute first command bound to one of KEYS without `umbra-mode'.

Check each KEY in sequence to see whether it would be bound if
`umbra-mode' were not active.  Execute the first command found;
if none is found, call `undefined'.

This function serves as the back end for the commands
`umbra-default-return-command', `umbra-default-tab-command', and
`umbra-default-kp-enter-command'."
  ;; We do everything possible to let the command called think it was
  ;; called via its usual binding, as doing otherwise can confuse some
  ;; commands.
  (let* ((key                 (seq-find #'umbra-default-binding keys))
	 (last-command-event  (if (stringp key) (string-to-char key) key)))
    (call-interactively
     (if key (umbra-default-binding key) #'undefined)
     (not :record-flag)
     (vector last-command-event))))

(defun umbra-default-return-command ()
  "Execute the command bound to RET without `umbra-mode'.

Since `umbra-mode' shadows ordinary mode-specific bindings, it
can be difficult to bind C-m (a.k.a. RET) in `umbra-mode'
separately from <return>.  This command attempts to alleviate
that difficulty by executing whatever command would be bound to
RET if `umbra-mode' were not active.

In the event that a command is actually bound to <return> outside
of `umbra-mode', it takes priority over the command bound to RET.

If you intend to bind C-m separately from <return> in
`umbra-mode-map' or any augmented keymap, bind this command to
<return> in `umbra-mode-map'."
  (interactive)
  (umbra-default-command (kbd "<return>")
			 (kbd "RET")))

(defun umbra-default-kp-enter-command ()
  "Execute the command bound to <kp-enter> without `umbra-mode'.

This is ordinarily the same as `umbra-default-return-command',
but will preserve any command specifically bound to <kp-enter>
rather than to RET or <return>."
  (interactive)
  (umbra-default-command (kbd "<kp-enter>")
			 (kbd "<return>")
			 (kbd "RET")))

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
  (umbra-default-command (kbd "TAB")))


;;;; Unloading
;;;;==========
(defun umbra-unload-function ()
  "Undo changes made to Emacs for `umbra-mode'.

Changes reversed are as follows:
- Addition of `umbra--update-major-mode' to `after-change-major-mode-hook'
- Keymaps added to `emulation-mode-map-alists'
- Advice for `bind-keys' adding support for :umbra and :penumbra
- All umbra and penumbra keymaps" 
  (remove-hook 'after-change-major-mode-hook #'umbra--update-major-mode)
  (dolist (elt '(umbra-overriding-map-alist
                 umbra-minor-mode-map-alist
                 umbra-local-map-alist))
    (aph/assq-delete-in emulation-mode-map-alists elt))
  (advice-remove 'bind-keys #'umbra--bind-keys-advice)
  (mapatoms (lambda (sym)
              (dolist (prefix '("umbra-mode-map:"
                                "umbra-overriding-mode-map:"))
                (when (string-prefix-p prefix (symbol-name sym))
                  (unintern sym)))))
  ;; Return nil so standard unloading continues
  nil)

(provide 'umbra)
;;; umbra.el ends here
