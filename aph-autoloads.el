;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aph-org" "/home/Aaron/sync/emacs/init/aph-org.el"
;;;;;;  "d93c75e741e9a17988e901663457a451")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/aph-org.el

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

;;;### (autoloads nil "aph-theme" "/home/Aaron/sync/emacs/init/aph-theme.el"
;;;;;;  "ed632e0fde31778406f0f6bf9beeedbb")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/aph-theme.el

(and load-file-name (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil "aph-window" "/home/Aaron/sync/emacs/init/aph-window.el"
;;;;;;  "f851cea49a337046fbfcb85d175a019a")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/aph-window.el

(defvar aph/help-window-names '("*Help*" "*Apropos*" "*Messages*" "*Completions*" "*Command History*" "*Compile-Log*" "*disabled command*") "\
Names of buffers that `aph/quit-help-windows' should quit.")

;;;***

;;;### (autoloads nil "browse-url-prefix" "/home/Aaron/sync/emacs/init/browse-url-prefix.el"
;;;;;;  "e9484922f0e99b8f077428ba8f95e9a0")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/browse-url-prefix.el

(autoload 'browse-url-prefix "browse-url-prefix" "\
Browse URL using a browser determined by the prefix arg.

With a prefix arg, the function

Interactively, prompt the user for URL, using any URL at point as
a default.

\(fn URL &rest ARGS)" t nil)

;;;***

;;;### (autoloads nil "cde" "/home/Aaron/sync/emacs/init/cde.el"
;;;;;;  "e1af80f8452e96f551a187f9734168b7")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/cde.el

(autoload 'cde "cde" "\
Count the numbers in RANGES.

Here, RANGES may either be a comma-separated string of hyphenated
ranges, e.g. \"1-5,7,8-15\", or a list encoding the same
information, e.g., '((1 5) 7 (8 15)). For both of the examples above,
cde will return 14.

Additionally, the alphabetic character \"A\" and \"B\" are
interpreted as sides of a page, with the page size given by
`cde-page-size'.  Thus if `cde-page-size' is 50 (the default),
then the string \"5A\" will be interpreted as the integer 5,
while the string \"5B\" will be interpreted as 55.  This feature
is unavailable if RANGES is presented as a list.

\(fn RANGES)" nil nil)

(autoload 'cde-format "cde" "\
Convert word at point with `cde'.

If the text at point looks like a suitable input for `cde',
replace it with a string of the form

    N (RANGES)

where RANGES is the original word and N is the result of calling
`cde' on RANGES.  The return value is the text inserted.

If the text at point is not suitable for `cde', do nothing and
return nil.  Interactively, or with VERBOSE non-nil, print an
explanatory message.

\(fn &optional VERBOSE)" t nil)

;;;***

;;;### (autoloads nil "elfeed-lens" "/home/Aaron/sync/emacs/init/elfeed-lens.el"
;;;;;;  "a75b0d70278c48c26130cbd043869a06")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/elfeed-lens.el

(autoload 'elfeed-lens-cycle "elfeed-lens" "\
Apply the next filter in `elfeed-lens-list'.

If the current search filter is an element of `elfeed-lens-list',
apply the filter immediately following that one in the list,
looping back to the beginning if necessary.

If the current search filter is not in `elfeed-lens-list', just
apply the first filter in the list.

If `elfeed-lens-list' is empty, just apply the default filter.

Return the filter applied.  When called interactively or the
optional VERBOSE parameter is non-nil, also print a message
informing the user of the newly applied filter.

\(fn &optional VERBOSE)" t nil)

;;;***

;;;### (autoloads nil "enumerate" "/home/Aaron/sync/emacs/init/enumerate.el"
;;;;;;  "7868dfcf91b7b13873477e51c234393a")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/enumerate.el

(autoload 'enumerate-lines "enumerate" "\
Insert a number for each line in the region.
Non-interactively, act on the text between BEG and END.

If the region is not active, act on the entire buffer.

If the region to be acted on starts or ends in the middle of a
line, a line number will still be inserted for that line.

If OFFSET is supplied (interactively, as a numeric prefix
argument), it is used as a starting point for the numbering;
otherwise, it defaults to 1.

If EOL is non-nil, the numbers will be appended to the end of
each line instead of being prepended to the beginning.

The SEP argument is the string separating the line number from
the rest of the line; i.e., it will ordinarily follow the number
but precedes it if EOL is supplied.  This defaults to a single
space.

The FORMAT-STR argument can be used to configure how the
numbers are displayed.  This is a string similar to those used in
the `format' function, but all ordinary format specifiers should
be double-escaped (e.g., \"%%d\"), and the special escape \"%n\"
will be replaced with the maximum number of digits of any line
number before the other escapes are interpreted.  The default
value for FORMAT-STR is \"%%%nd\".

\(fn BEG END &optional OFFSET EOL SEP FORMAT-STR)" t nil)

(autoload 'enumerate-alpha "enumerate" "\
Number lines in region according to alphabetic order.

As `enumerate-lines', except lines are numbered according to
their alphabetic order instead of their position, and the option
to put the number at the end of the line is unavailable.

\(fn BEG END &optional OFFSET SEP FORMAT-STRING)" t nil)

;;;***

;;;### (autoloads nil "forms-aux" "/home/Aaron/sync/emacs/init/forms-aux.el"
;;;;;;  "948904b72b9145c3969c433d6acc4e73")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/forms-aux.el

(autoload 'forms-aux-open-file "forms-aux" "\
Open the auxiliary file for the current record.

The filename to open is the current record's value for the field
numbered by `forms-aux-field'.  Open this file, if it exists, in
a separate window, and kill any other buffer previously created
by this function. 

Return the newly opened buffer, but do not select it.

If `forms-aux-field' is nil, do nothing.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "forms-random" "/home/Aaron/sync/emacs/init/forms-random.el"
;;;;;;  "509fdb0d449022f899372390ed248b54")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/forms-random.el

(autoload 'forms-random-record "forms-random" "\
Go to a randomly selected record in current database.

\(fn)" t nil)

(autoload 'forms-random-record-weighted "forms-random" "\
As `forms-random-record', but die is weighted.

Interpret the field specified by the variable
`forms-random-weight-field' as a weighting factor, using the
value of `forms-random-weight-transform' to transform this into
an integer.

If `forms-random-weight-field' is nil, treat all weights as
equal; i.e., behave identically to `forms-random-record'.

The chance of selecting any particular record R is then n/N,
where n is the value R has for the weighting field and N is the
total of this field across all records in the database.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-forms" "/home/Aaron/sync/emacs/init/helm-forms.el"
;;;;;;  "da052f702b3e6703cf3f2aed3c6bef26")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/helm-forms.el

(autoload 'helm-forms-records "helm-forms" "\
A `helm' command for browsing records in `forms-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mode-family" "/home/Aaron/sync/emacs/init/mode-family.el"
;;;;;;  "3c02d24d863b116e707b18cd7fb87045")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/mode-family.el

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

;;;### (autoloads nil "multitheme" "/home/Aaron/sync/emacs/init/multitheme.el"
;;;;;;  "7e75ddd89592bd8915716636f1e849a9")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/multitheme.el

(autoload 'multitheme-cycle "multitheme" "\
Cycle between the themes in `multitheme-base-theme-list'.
If none of these themes is currently active, instead enable the
first element of `multitheme-base-theme-list'.

Also re-enable `multitheme-overtheme' so it remains \"on top\" of
the base theme.

If a theme to be enabled is not yet defined, attempt to load it
first (using `load-theme').  Respect `custom-safe-themes'.

After all theme changes have been made, run
`multitheme-base-change-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "source-lock" "/home/Aaron/sync/emacs/init/source-lock.el"
;;;;;;  "2ff105959fe4be6617ad9a9a795a529b")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/source-lock.el

(defvar source-lock-mode nil "\
Non-nil if Source-Lock mode is enabled.
See the command `source-lock-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `source-lock-mode'.")

(custom-autoload 'source-lock-mode "source-lock" nil)

(autoload 'source-lock-mode "source-lock" "\
Mode to make source files read-only.

When enabled, any file in one of the directories listed in
`source-lock-directories' will be opened in read-only mode.  If
`source-lock-protect-packages-p' is non-nil, then
`package-user-dir' is similarly protected.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "umbra" "/home/Aaron/sync/emacs/init/umbra.el"
;;;;;;  "b1885ac57d88517a321f5526bd25bd2c")
;;; Generated autoloads from /home/Aaron/sync/emacs/init/umbra.el

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

;;;### (autoloads nil nil ("/home/Aaron/sync/emacs/init/aph-advice.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-align.el" "/home/Aaron/sync/emacs/init/aph-autoloads.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-browse-url.el" "/home/Aaron/sync/emacs/init/aph-comparators.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-dash.el" "/home/Aaron/sync/emacs/init/aph-ert.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-face-remap.el" "/home/Aaron/sync/emacs/init/aph-files.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-frame.el" "/home/Aaron/sync/emacs/init/aph-framewin.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-haskell.el" "/home/Aaron/sync/emacs/init/aph-helm-projectile.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-helm.el" "/home/Aaron/sync/emacs/init/aph-help.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-ielm.el" "/home/Aaron/sync/emacs/init/aph-iimage.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-info.el" "/home/Aaron/sync/emacs/init/aph-keypad.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-latex.el" "/home/Aaron/sync/emacs/init/aph-lexical.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-lisp-mode.el" "/home/Aaron/sync/emacs/init/aph-message.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-org-agenda.el" "/home/Aaron/sync/emacs/init/aph-org-capture.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-org-table.el" "/home/Aaron/sync/emacs/init/aph-org.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-outline.el" "/home/Aaron/sync/emacs/init/aph-page.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-plist.el" "/home/Aaron/sync/emacs/init/aph-rect.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-shr.el" "/home/Aaron/sync/emacs/init/aph-silence.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-simple.el" "/home/Aaron/sync/emacs/init/aph-smartparens.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-subr.el" "/home/Aaron/sync/emacs/init/aph-symbol.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-theme.el" "/home/Aaron/sync/emacs/init/aph-which-func.el"
;;;;;;  "/home/Aaron/sync/emacs/init/aph-window.el" "/home/Aaron/sync/emacs/init/browse-url-prefix.el"
;;;;;;  "/home/Aaron/sync/emacs/init/cde.el" "/home/Aaron/sync/emacs/init/chimera.el"
;;;;;;  "/home/Aaron/sync/emacs/init/cygwinize.el" "/home/Aaron/sync/emacs/init/elfeed-barb.el"
;;;;;;  "/home/Aaron/sync/emacs/init/elfeed-lens.el" "/home/Aaron/sync/emacs/init/elfeed-link.el"
;;;;;;  "/home/Aaron/sync/emacs/init/enumerate.el" "/home/Aaron/sync/emacs/init/formation.el"
;;;;;;  "/home/Aaron/sync/emacs/init/forms-aux.el" "/home/Aaron/sync/emacs/init/forms-barb.el"
;;;;;;  "/home/Aaron/sync/emacs/init/forms-random.el" "/home/Aaron/sync/emacs/init/helm-forms.el"
;;;;;;  "/home/Aaron/sync/emacs/init/init-core.el" "/home/Aaron/sync/emacs/init/init-draft.el"
;;;;;;  "/home/Aaron/sync/emacs/init/init-org-agenda.el" "/home/Aaron/sync/emacs/init/init-org-capture.el"
;;;;;;  "/home/Aaron/sync/emacs/init/init-org.el" "/home/Aaron/sync/emacs/init/jerk.el"
;;;;;;  "/home/Aaron/sync/emacs/init/liberate-key.el" "/home/Aaron/sync/emacs/init/mode-family.el"
;;;;;;  "/home/Aaron/sync/emacs/init/morgue.el" "/home/Aaron/sync/emacs/init/multitheme.el"
;;;;;;  "/home/Aaron/sync/emacs/init/source-lock.el" "/home/Aaron/sync/emacs/init/umbra.el"
;;;;;;  "aph-advice.el" "aph-align.el" "aph-browse-url.el" "aph-comparators.el"
;;;;;;  "aph-dash.el" "aph-ert.el" "aph-face-remap.el" "aph-files.el"
;;;;;;  "aph-frame.el" "aph-framewin.el" "aph-haskell.el" "aph-helm-projectile.el"
;;;;;;  "aph-helm.el" "aph-help.el" "aph-ielm.el" "aph-iimage.el"
;;;;;;  "aph-info.el" "aph-keypad.el" "aph-latex.el" "aph-lexical.el"
;;;;;;  "aph-lisp-mode.el" "aph-message.el" "aph-org-agenda.el" "aph-org-capture.el"
;;;;;;  "aph-org-table.el" "aph-outline.el" "aph-page.el" "aph-plist.el"
;;;;;;  "aph-rect.el" "aph-shr.el" "aph-silence.el" "aph-simple.el"
;;;;;;  "aph-smartparens.el" "aph-subr.el" "aph-symbol.el" "aph-which-func.el"
;;;;;;  "chimera.el" "cygwinize.el" "elfeed-barb.el" "elfeed-link.el"
;;;;;;  "formation.el" "forms-barb.el" "init-core.el" "init-draft.el"
;;;;;;  "init-org-agenda.el" "init-org-capture.el" "init-org.el"
;;;;;;  "jerk.el" "liberate-key.el" "morgue.el") (22334 17560 880240
;;;;;;  100000))

;;;***

(provide 'aph-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aph-autoloads.el ends here
