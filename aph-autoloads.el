;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aph-mpc" "aph-mpc.el" (22194 22352 298961
;;;;;;  500000))
;;; Generated autoloads from aph-mpc.el

(autoload 'cde "aph-mpc" "\
Count the numbers in RANGES.

Here, RANGES may either be a comma-separated string of hyphenated
ranges, e.g. \"1-5,7,8-15\", or a list encoding the same
information, e.g., '((1 5) 7 (8 15)). For both of the examples above,
cde will return 14.

\(fn RANGES)" nil nil)

;;;***

;;;### (autoloads nil "aph-org" "aph-org.el" (22257 53869 994247
;;;;;;  700000))
;;; Generated autoloads from aph-org.el

(autoload 'aph/org-spin-basic "aph-org" "\
Move point to a random child of heading at point.
Return point.

\(fn)" t nil)

(autoload 'aph/org-spin-weighted "aph-org" "\
As `aph/org-spin-basic', weighted by property WEIGHT-PROP.

The parameter WEIGHT-PROP should be the name of a property.
Non-negative numeric values for that property are treated as
weights for the spin. Non-numeric and negative values are treated
as zero.

When called interactively or if WEIGHT-PROP is
omitted,`aph/org-spin-weight-property' is used.

\(fn &optional WEIGHT-PROP)" t nil)

(autoload 'aph/org-goto-last-refile "aph-org" "\
Goto last Org-mode item refiled.

This has the same effect as supplying a C-u C-u prefix argument
to `org-agenda-refile'.  It is intended for use globally, where a
keybinding for that function is not appropriate.

\(fn)" t nil)

(autoload 'aph/org-eww-store-link "aph-org" "\
Store the current eww url as an Org-Mode link.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "aph-window" "aph-window.el" (22217 60391 0
;;;;;;  0))
;;; Generated autoloads from aph-window.el

(defvar aph/help-window-names '("*Help*" "*Apropos*" "*Messages*" "*Completions*" "*Command History*" "*Compile-Log*" "*disabled command*") "\
Names of buffers that `aph/quit-help-windows' should quit.")

;;;***

;;;### (autoloads nil "mode-family" "mode-family.el" (22277 35665
;;;;;;  55619 500000))
;;; Generated autoloads from mode-family.el

(autoload 'mode-family-create "mode-family" "\
Define FAMILY as a mode family.
If one already exists, do nothing.

A mode family is a way to designate several otherwise-unrelated
modes (major or minor) as sharing some particular characteristic.
Mode families are named using symbols in the standard obarray,
but this does not use either a symbol's variable slot or its
function slot, so the same symbol can name both a mode family and
a variable and/or function.

To associate a mode with a mode family, see `mode-family-add';
this creates the family if it doesn't already exist.  To remove a
mode from a family, see `mode-family-remove'.  To check for the
existence of a family, see `mode-family-exists-p'.  To check
whether a mode is part of a family, see `mode-family-member-p',
`mode-family-list-members', or `mode-family-list-families'.

Each mode family has a hook, named `FAMILY-family-hook', and all
modes associated with a family (or derived from such a mode) run
this hook along with their individual mode hooks.

In the event that a variable already exists with the same name as
the hook variable for the new family, it will be \"stolen\" by
the mode family (without warning!), but its value will not be
changed; this is necessary in order to allow for out-of-order
hooks.

\(fn FAMILY)" nil nil)

(put 'mode-family-create 'lisp-indent-function 'defun)

(autoload 'mode-family-add "mode-family" "\
Add MODE to FAMILY.  If FAMILY doesn't exist, create it.
If MODE is already a member of FAMILY, do nothing.

See the documentation for `mode-family-create' for more information.

\(fn MODE FAMILY)" nil nil)

;;;***

;;;### (autoloads nil "umbra" "umbra.el" (22268 697 871345 400000))
;;; Generated autoloads from umbra.el

(autoload 'umbra-keymap "umbra" "\
Return umbra keymap corresponding to MODE for `umbra-mode'.

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
keymaps are named like 'umbra-overriding-mode-map:MODE.

\(fn MODE &optional PENUMBRA)" nil nil)

(defvar umbra-mode nil "\
Non-nil if Umbra mode is enabled.
See the command `umbra-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `umbra-mode'.")

(custom-autoload 'umbra-mode "umbra" nil)

(autoload 'umbra-mode "umbra" "\
Mode for reversible keybindings.

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
penumbra map instead.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("aph-advice.el" "aph-align.el" "aph-bind-key.el"
;;;;;;  "aph-browse-url.el" "aph-comparators.el" "aph-cygwin.el"
;;;;;;  "aph-dash.el" "aph-elfeed.el" "aph-ert.el" "aph-face-remap.el"
;;;;;;  "aph-files.el" "aph-font-lock.el" "aph-forms.el" "aph-frame.el"
;;;;;;  "aph-framewin.el" "aph-haskell.el" "aph-helm-forms.el" "aph-helm-projectile.el"
;;;;;;  "aph-helm.el" "aph-help.el" "aph-ielm.el" "aph-iimage.el"
;;;;;;  "aph-info.el" "aph-keypad.el" "aph-keys.el" "aph-latex.el"
;;;;;;  "aph-lexical.el" "aph-lisp-mode.el" "aph-message.el" "aph-number-lines.el"
;;;;;;  "aph-org-agenda.el" "aph-org-capture.el" "aph-org-table.el"
;;;;;;  "aph-outline.el" "aph-page.el" "aph-plist.el" "aph-rect.el"
;;;;;;  "aph-shr.el" "aph-silence.el" "aph-simple.el" "aph-smartparens.el"
;;;;;;  "aph-subr.el" "aph-symbol.el" "aph-theme.el" "aph-w32.el"
;;;;;;  "aph-which-func.el" "init-core.el" "init-draft.el" "init-elfeed.el"
;;;;;;  "init-org-agenda.el" "init-org-capture.el" "init-org.el")
;;;;;;  (22277 35673 167532 500000))

;;;***

(provide 'aph-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aph-autoloads.el ends here
