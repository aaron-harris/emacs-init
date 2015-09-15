;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aph-commands" "aph-commands.el" (22008 25892
;;;;;;  729621 0))
;;; Generated autoloads from aph-commands.el

(autoload 'aph/apropos-function "aph-commands" "\
As `apropos-command', but show all functions by default.
The behavior of a prefix argument is inverted, so supplying a
prefix argument will show only commands (and override a non-nil
value for `apropos-do-all')

\(fn PATTERN &optional COMMANDS-ONLY VAR-PREDICATE)" t nil)

(autoload 'aph/delete-frame-or-exit "aph-commands" "\
Delete this frame. With only one frame, exit Emacs.

When there is more than one visible frame, run `delete-frame'.
Otherwise, exit Emacs with `save-buffers-kill-terminal'.

Any prefix ARG is passed to `save-buffers-kill-terminal' in the
single-frame case and ignored otherwise.

\(fn &optional ARG)" t nil)

(autoload 'aph/kill-active-buffer "aph-commands" "\
Kill the active buffer.

With a prefix argument, choose the buffer to kill (as the
standard `kill-buffer').

\(fn &optional CHOOSE)" t nil)

(autoload 'aph/kp-enter-newline-toggle "aph-commands" "\
Toggle whether <kp-enter> should act like C-n instead of enter.
Accomplish this by updating the entry for <kp-enter> in
`function-key-map'.  If this entry is something other than these
two keys, restore it to the default mapping (enter, i.e. C-m).

When called interactively, or when the optional parameter is
supplied, supply feedback messages.  Always print a message when
the entry being overridden is unexpected.

For convenience, return the new mapping.

\(fn &optional VERBOSE)" t nil)

(autoload 'aph/newline "aph-commands" "\
As `newline', with support for negative argument.
An argument of -N calls `join-line' N times.

\(fn N)" t nil)

(autoload 'aph/open-line "aph-commands" "\
As `open-line', with support for negative argument.
An argument of -N calls `join-line' with an argument N times.

\(fn N)" t nil)

(autoload 'aph/other-window-backwards "aph-commands" "\
As `other-window' but reversed.

\(fn COUNT &optional ALL-FRAMES)" t nil)

(autoload 'aph/scroll-down-by-line "aph-commands" "\
As `scroll-down-command', but ARG defaults to 1.

Also, a negative prefix argument is treated as -1, scrolling only
one line upward.

\(fn &optional ARG)" t nil)

(autoload 'aph/scroll-up-by-line "aph-commands" "\
As `scroll-up-command', but ARG defaults to 1.

Also, a negative prefix argument is treated as -1, scrolling only
one line downward.

\(fn &optional ARG)" t nil)

(autoload 'aph/quit-help-windows "aph-commands" "\
Quit all windows with help-like buffers.

Call `quit-windows-on' for every buffer named in
`aph/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames.

\(fn &optional KILL FRAME)" t nil)

(autoload 'aph/sum-parens-in-region "aph-commands" "\
Sum all parenthesized numbers in region and echo the result.
If the region is not active, sum all parenthesized numbers in
active buffer.

See `aph/sum-parens' to get similar functionality from elisp.

\(fn START END)" t nil)

;;;***

;;;### (autoloads nil "aph-ielm" "aph-ielm.el" (22008 19541 730390
;;;;;;  800000))
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

;;;### (autoloads nil "aph-mpc" "aph-mpc.el" (22008 32806 532682
;;;;;;  400000))
;;; Generated autoloads from aph-mpc.el

(autoload 'aph/yank-access-inline "aph-mpc" "\
Yank the most recent kill, cleaning up MS Access formatting.

Specifically, collapse all whitespace in the most recent kill to
spaces, remove the first word of the kill entirely, then
yank. Also push the result back onto the kill ring (not replacing
the original).

This function is designed to clean up text copied as a rectangle
from a Microsoft Access datasheet. In these circumstances, the
cell contents are delimited by newlines and the field name is
inserted at the top, which can make doing calculations on the
data awkward.

\(fn)" t nil)

(autoload 'cde "aph-mpc" "\
Count the numbers in RANGES.

Here, RANGES may either be a comma-separated string of hyphenated
ranges, e.g. \"1-5,7,8-15\", or a list encoding the same
information, e.g., '((1 5) 7 (8 15)). For both of the examples above,
cde will return 14.

\(fn RANGES)" nil nil)

;;;***

;;;### (autoloads nil "aph-org" "aph-org.el" (22008 25643 751950
;;;;;;  0))
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

(autoload 'aph/org-capture-in-popout-frame "aph-org" "\
As `org-capture', but do all work in a new frame.

This function by itself doesn't clean up the frame following
capture.  To do that, add `aph/org-capture-delete-capture-frame'
to `org-capture-after-finalize-hook'.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'aph/org-capture-delete-capture-frame "aph-org" "\
Delete a frame named \"Capture\".
For use in `org-capture-after-finalize-hook' to clean up after
`aph/org-capture-in-popout-frame'.

\(fn)" nil nil)

(autoload 'aph/org-goto-last-refile "aph-org" "\
Goto last Org-mode item refiled.

This has the same effect as supplying a C-u C-u prefix argument
to `org-agenda-refile'.  It is intended for use globally, where a
keybinding for that function is not appropriate.

\(fn)" t nil)

(autoload 'aph/org-agenda "aph-org" "\
As `org-agenda', and automatically refresh sticky agendas.

\(fn)" t nil)

(autoload 'aph/org-eww-store-link "aph-org" "\
Store the current eww url as an Org-Mode link.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "aph-shell" "aph-shell.el" (22008 32355 370444
;;;;;;  300000))
;;; Generated autoloads from aph-shell.el

(autoload 'aph/hg-commit "aph-shell" "\
Run the shell command \"hg commit\" asynchronously.
With a prefix argument, prompt for additional flags.

\(fn &optional FLAGS)" t nil)

(autoload 'aph/hg-log "aph-shell" "\
Run the shell command \"hg log\".
If an argument N is provided, instead run \"hg log -l N\".

\(fn &optional N)" t nil)

(autoload 'aph/hg-status "aph-shell" "\
Run the shell command \"hg status\".

\(fn)" t nil)

;;;***

;;;### (autoloads nil "aph-theme" "aph-theme.el" (22008 32818 732466
;;;;;;  200000))
;;; Generated autoloads from aph-theme.el

(autoload 'aph/theme-cycle "aph-theme" "\
Cycle between the themes in `aph/theme-list'.
If none of these themes is currently active, instead load the
first element of `aph/theme-list'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("aph-comparators.el" "aph-framewin.el"
;;;;;;  "aph-hooks.el" "aph-keys.el" "aph-lib.el" "aph-org-agenda.el"
;;;;;;  "aph-require.el" "init-ahk.el" "init-color-identifiers.el"
;;;;;;  "init-core.el" "init-docview.el" "init-draft.el" "init-gnus.el"
;;;;;;  "init-ido.el" "init-keys.el" "init-latex.el" "init-lisp-clojure.el"
;;;;;;  "init-lisp-emacs.el" "init-lisp.el" "init-misc.el" "init-org-agenda.el"
;;;;;;  "init-org-capture.el" "init-org.el" "init-package.el" "init-smartparens.el"
;;;;;;  "init-startup.el" "init-visible-mark.el") (22008 32836 707819
;;;;;;  200000))

;;;***

(provide 'aph-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aph-autoloads.el ends here
