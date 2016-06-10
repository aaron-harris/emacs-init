;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aph-helm" "aph-helm.el" (22335 35426 611881
;;;;;;  300000))
;;; Generated autoloads from aph-helm.el

(autoload 'aph/helm-browse-project "aph-helm" "\
As `helm-browse-project', but truncate lines.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "aph-helm-projectile" "aph-helm-projectile.el"
;;;;;;  (22335 35414 770211 300000))
;;; Generated autoloads from aph-helm-projectile.el

(autoload 'aph/helm-projectile "aph-helm-projectile" "\
As `helm-projectile', but truncate lines.

\(fn &optional ARG)" t nil)

(autoload 'aph/helm-projectile-grep "aph-helm-projectile" "\
As `helm-projectile-grep', but suspend updates initially.

\(fn &optional DIR)" t nil)

;;;***

;;;### (autoloads nil "aph-ielm" "aph-ielm.el" (22342 12714 23786
;;;;;;  300000))
;;; Generated autoloads from aph-ielm.el

(autoload 'aph/ielm-copy-last-output "aph-ielm" "\
Copy the last output produced by `ielm' to the kill ring.

With argument N > 0, instead copy the Nth last output.
With argument N < 0, copy the Nth output since last clear.
With argument N = 0, do nothing.

Return the newly copied string, or nil if nothing was
copied (e.g., if the argument is greater than the number of
uncleared outputs).

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "aph-iimage" "aph-iimage.el" (22343 25955 472029
;;;;;;  800000))
;;; Generated autoloads from aph-iimage.el

(autoload 'aph/iimage-refresh "aph-iimage" "\
Refresh inline images in current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "aph-info" "aph-info.el" (22343 12844 97830
;;;;;;  300000))
;;; Generated autoloads from aph-info.el

(autoload 'aph/info-mode-or-clone-buffer "aph-info" "\
Enter info mode or clone info buffer.

In an info buffer when no prefix argument has been supplied,
clone the buffer (as `clone-buffer').  Otherwise, enter info
mode (as `info').

\(fn PREFIX)" t nil)

(autoload 'aph/Info-final-menu-item "aph-info" "\
Go to the node of the last menu item.

This command duplicates the functionality of the 0 key in the
standalone info application.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "aph-org" "aph-org.el" (22361 46402 476865
;;;;;;  100000))
;;; Generated autoloads from aph-org.el

(autoload 'aph/org-goto-last-refile "aph-org" "\
Goto last Org-mode item refiled.

This has the same effect as supplying a C-u C-u prefix argument
to `org-agenda-refile'.  It is intended for use globally, where a
keybinding for that function is not appropriate.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "aph-org-table" "aph-org-table.el" (22360 37778
;;;;;;  619722 200000))
;;; Generated autoloads from aph-org-table.el

(autoload 'aph/org-table-clear-row-forward "aph-org-table" "\
Erase contents of table cells from point to end of row.

If point is not inside an Org table, signal an error.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "aph-theme" "aph-theme.el" (22316 62223 201410
;;;;;;  400000))
;;; Generated autoloads from aph-theme.el

(and load-file-name (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil "aph-window" "aph-window.el" (22217 60391 0
;;;;;;  0))
;;; Generated autoloads from aph-window.el

(defvar aph/help-window-names '("*Help*" "*Apropos*" "*Messages*" "*Completions*" "*Command History*" "*Compile-Log*" "*disabled command*") "\
Names of buffers that `aph/quit-help-windows' should quit.")

;;;***

;;;### (autoloads nil "browse-url-prefix" "browse-url-prefix.el"
;;;;;;  (22330 17349 411909 400000))
;;; Generated autoloads from browse-url-prefix.el

(autoload 'browse-url-prefix "browse-url-prefix" "\
Browse URL using a browser determined by the prefix arg.

With a prefix arg, the function

Interactively, prompt the user for URL, using any URL at point as
a default.

\(fn URL &rest ARGS)" t nil)

;;;***

;;;### (autoloads nil "canary" "canary.el" (22341 65219 709867 600000))
;;; Generated autoloads from canary.el

(autoload 'canary "canary" "\
Print a message containing ARGS.

\(fn &rest ARGS)" nil nil)

(autoload 'canary-hooks "canary" "\
Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks.

\(fn COMMAND &optional VERBOSE)" t nil)

;;;***

;;;### (autoloads nil "cde" "cde.el" (22330 17248 261613 200000))
;;; Generated autoloads from cde.el

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

;;;### (autoloads nil "clean-eval" "clean-eval.el" (22342 14480 483921
;;;;;;  600000))
;;; Generated autoloads from clean-eval.el

(defvar clean-eval-mode nil "\
Non-nil if Clean-Eval mode is enabled.
See the command `clean-eval-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `clean-eval-mode'.")

(custom-autoload 'clean-eval-mode "clean-eval" nil)

(autoload 'clean-eval-mode "clean-eval" "\
Mode to clean evaluation output.

When enabled, evaluating elisp (e.g., with `eval-expression' or
via `ielm') will not produce extra \"junk output\".  For example,
you would see

    (+ 1 1)
    => 2

rather than

    (+ 1 1)
    => 2 (#o2, #x2, ?\\C-b).

Note that this is accomplished via advice on the function
`eval-expression-print-format'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "elfeed-lens" "elfeed-lens.el" (22330 16992
;;;;;;  645014 100000))
;;; Generated autoloads from elfeed-lens.el

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

;;;### (autoloads nil "enumerate" "enumerate.el" (22306 21552 766542
;;;;;;  300000))
;;; Generated autoloads from enumerate.el

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

;;;### (autoloads nil "family-local" "family-local.el" (22340 54590
;;;;;;  272672 200000))
;;; Generated autoloads from family-local.el

(autoload 'setq-family-local "family-local" "\
Set each VAR to VALUE in FAMILY.

\(fn FAMILY [VAR VALUE] ...)" nil t)

(put 'setq-family-local 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil "fixed-scale" "fixed-scale.el" (22361 61750
;;;;;;  797020 400000))
;;; Generated autoloads from fixed-scale.el

(defvar fixed-scale-mode nil "\
Non-nil if Fixed-Scale mode is enabled.
See the command `fixed-scale-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fixed-scale-mode'.")

(custom-autoload 'fixed-scale-mode "fixed-scale" nil)

(autoload 'fixed-scale-mode "fixed-scale" "\
Global minor mode to make `text-scale-mode' stickier.

Sometimes a command will remove text scaling as a byproduct of
its function.  This mode attempts to rectify that.  To use it,
you must add commands that reset the text scale to the variable
`fixed-scale-command-list'.  Then, whenever one of those commands
is executed while `fixed-scale-mode' is enabled (it is enabled by
default, if the module `fixed-scale' has been loaded),
`fixed-scale-mode' will remember the proper text scaling and
restore it after the command is finished.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "forms-aux" "forms-aux.el" (22334 5858 341350
;;;;;;  800000))
;;; Generated autoloads from forms-aux.el

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

;;;### (autoloads nil "forms-random" "forms-random.el" (22334 14569
;;;;;;  877195 600000))
;;; Generated autoloads from forms-random.el

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

;;;### (autoloads nil "helm-forms" "helm-forms.el" (22334 17550 859516
;;;;;;  200000))
;;; Generated autoloads from helm-forms.el

(autoload 'helm-forms-records "helm-forms" "\
A `helm' command for browsing records in `forms-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "kp-motion" "kp-motion.el" (22343 25518 640308
;;;;;;  400000))
;;; Generated autoloads from kp-motion.el

(autoload 'kp-motion-mode "kp-motion" "\
Simple minor mode enabling motion with enter key on keypad.

When this mode is enabled, the enter key on the
keypad (`<kp-enter>') will be used for downward
motion (`next-line') instead of its normal function.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "mode-family" "mode-family.el" (22330 16407
;;;;;;  159146 400000))
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

;;;### (autoloads nil "multitheme" "multitheme.el" (22343 26772 57847
;;;;;;  800000))
;;; Generated autoloads from multitheme.el

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

;;;### (autoloads nil "org-child" "org-child.el" (22359 2268 388764
;;;;;;  800000))
;;; Generated autoloads from org-child.el

(autoload 'org-child-goto "org-child" "\
Goto the Nth child of heading at point.

Children are counted from 1.  If heading does not have N
children, return nil and do not move point; otherwise, return
point.

If N is zero, call `org-back-to-heading' and return point.

If N is negative, goto the (-N)th child from the end (so
\(org-child-goto -1) moves to the last child).

\(fn N)" t nil)

;;;***

;;;### (autoloads nil "org-display" "org-display.el" (22353 51591
;;;;;;  588280 800000))
;;; Generated autoloads from org-display.el

(autoload 'org-display-capture-in-popout-frame "org-display" "\
As `org-capture', but do all work in a new frame.

When capture is completed or aborted, the new frame will be
deleted.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'org-display-capture-in-whole-frame "org-display" "\
As `org-capture', but take over entire frame.

When capture is completed or aborted, the frame will be deleted.

This is designed to be used with the -e option for emacsclient,
where a frame has just been created that has no useful content in
it.  For normal usage, `org-display-capture-in-popout-frame'
probably makes more sense.

\(fn &optional GOTO KEYS)" t nil)

;;;***

;;;### (autoloads nil "org-smart-agenda" "org-smart-agenda.el" (22363
;;;;;;  18008 927786 300000))
;;; Generated autoloads from org-smart-agenda.el

(autoload 'org-smart-agenda "org-smart-agenda" "\
Display an Org-mode agenda based on current day and time.

See the variables `org-smart-agenda-views' and
`org-smart-agenda-workday' for configuration options.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-spin" "org-spin.el" (22359 2659 765605
;;;;;;  900000))
;;; Generated autoloads from org-spin.el

(autoload 'org-spin "org-spin" "\
Move point to a random child of heading at point.
Return point.

If point is before the first heading of the buffer, move point to
a random top-level heading.

\(fn)" t nil)

(autoload 'org-spin-weighted "org-spin" "\
As `org-spin', weighted by property WEIGHT-PROP.

The parameter WEIGHT-PROP should be the name of a property.
Numeric values for that property are treated as weights for the
spin.  Omitted weights default to 1 (but non-numeric values are
treated as 0).

If all weights are zero, then weights are ignored and the
selection is uniform, as in `org-spin'.

When called interactively or if WEIGHT-PROP is omitted, the value
of `org-spin-weight-property' is used.

\(fn &optional WEIGHT-PROP)" t nil)

;;;***

;;;### (autoloads nil "silence" "silence.el" (22341 65274 644268
;;;;;;  100000))
;;; Generated autoloads from silence.el

(autoload 'silence "silence" "\
Execute BODY silencing messages matching MSG-LIST.

Here MSG-LIST is a list of the same format as `silence-list'.
Its elements will be added to `silence-list' for the duration
of BODY, and `silence-enabled' will be treated as non-nil.

This is accomplished by advising `message'.  As `message' is a
primitive, not all messages can be silenced; calls from C code
may avoid being silenced.

\(fn MSG-LIST &rest BODY)" nil t)

(put 'silence 'lisp-indent-function '1)

(autoload 'silence-loading "silence" "\
Execute BODY silencing `load' messages.

Note that the mechanism used is unrelated to that used by the
`silence' macro.  Instead, we advise `load'.  As `load' is also a
primitive, the same caveat regarding C calls applies.

\(fn &rest BODY)" nil t)

(put 'silence-loading 'lisp-indent-function '0)

;;;***

;;;### (autoloads nil "source-lock" "source-lock.el" (22332 54401
;;;;;;  140316 900000))
;;; Generated autoloads from source-lock.el

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

;;;### (autoloads nil "umbra" "umbra.el" (22351 14864 363645 0))
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

;;;### (autoloads nil nil ("aph-advice.el" "aph-align.el" "aph-browse-url.el"
;;;;;;  "aph-comparators.el" "aph-dash.el" "aph-ert.el" "aph-files.el"
;;;;;;  "aph-framewin.el" "aph-haskell.el" "aph-help.el" "aph-latex.el"
;;;;;;  "aph-lisp-mode.el" "aph-org-agenda.el" "aph-org-capture.el"
;;;;;;  "aph-outline.el" "aph-page.el" "aph-plist.el" "aph-rect.el"
;;;;;;  "aph-shr.el" "aph-silence.el" "aph-simple.el" "aph-smartparens.el"
;;;;;;  "aph-subr.el" "aph-symbol.el" "aph-which-func.el" "chimera.el"
;;;;;;  "cygwinize.el" "elfeed-barb.el" "elfeed-link.el" "formation.el"
;;;;;;  "forms-barb.el" "init-core.el" "init-draft.el" "init-org-agenda.el"
;;;;;;  "init-org-capture.el" "init-org.el" "jerk.el" "lexy.el" "liberate-key.el"
;;;;;;  "morgue.el" "org-agenda-sticky.el" "org-eww.el" "org-match.el"
;;;;;;  "org-multitheme.el" "vizier-helm.el" "vizier.el") (22363
;;;;;;  18013 830208 300000))

;;;***

(provide 'aph-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aph-autoloads.el ends here
