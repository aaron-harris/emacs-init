;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aph-commands" "aph-commands.el" (22058 26208
;;;;;;  291152 900000))
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
Otherwise, exit Emacs with `save-buffers-kill-terminal' after
confirming this with user.

If a prefix ARG is supplied, ignore it in the multiple-frame
case.  Otherwise, bypass confirmation and pass the argument to
`save-buffers-kill-terminal'.

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

(autoload 'aph/yank-rectangle-from-kill-ring "aph-commands" "\
Yank the top of kill ring as a rectangle.
Make the \"last killed rectangle\" be the top entry of the kill
ring, then yank that rectangle at point.

With \\[universal-argument] as argument, just save the top entry
of the kill ring as a rectangle, without yanking.  Print a
message to that effect.  When called from elisp, this message is
suppressed unless the optional argument VERBOSE is supplied.
 
With argument N, save the Nth most recent kill instead of the
most recent.

\(fn &optional ARG VERBOSE)" t nil)

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

;;;### (autoloads nil "aph-elfeed" "aph-elfeed.el" (22071 58266 146543
;;;;;;  500000))
;;; Generated autoloads from aph-elfeed.el

(autoload 'aph/elfeed-search-show-entry "aph-elfeed" "\
As `elfeed-search-show-entry', but intelligently follow links.

If ENTRY is tagged with the 'link tag, presume that the text of
ENTRY will be incomplete and open the url in ENTRY's link field.
If the optional parameter EXTERNAL is supplied (interactively, with a
prefix argument), use an external browser; otherwise, use `eww'.

If ENTRY doesn't have the \"link\" tag, call `elfeed-show-entry'.
In this case, ignore the EXTERNAL parameter.

\(fn ENTRY &optional EXTERNAL)" t nil)

(autoload 'aph/elfeed-show-next "aph-elfeed" "\
As `elfeed-show-next', but intelligently follow links.
See `aph/elfeed-search-show-entry' for details.

\(fn &optional EXTERNAL)" t nil)

(autoload 'aph/elfeed-show-prev "aph-elfeed" "\
As `elfeed-show-prev', but intelligently follow links.
See `aph/elfeed-search-show-entry' for details.

\(fn &optional EXTERNAL)" t nil)

;;;***

;;;### (autoloads nil "aph-framewin" "aph-framewin.el" (22034 45614
;;;;;;  275659 800000))
;;; Generated autoloads from aph-framewin.el

(autoload 'aph/slide-buffer-forward "aph-framewin" "\
Slide active buffer to another window.

Display this buffer COUNT windows forward (in the same ordering
as `other-window'), skipping windows dedicated to their current
buffers, and display in this window the previous buffer displayed
here (using `switch-to-prev-buffer').

If the optional parameter RIDE is supplied, \"ride\" the buffer,
making its new window the selected one.

As a special case, if COUNT is zero, treat COUNT as 1 and RIDE as
t.  This allows the RIDE parameter to be used interactively.

\(fn &optional COUNT RIDE)" t nil)

(autoload 'aph/slide-buffer-backward "aph-framewin" "\
As `aph/slide-buffer-forward' with direction reversed.

\(fn &optional COUNT RIDE)" t nil)

(autoload 'aph/swap-buffer-forward "aph-framewin" "\
Swap active buffer with that in another window.

Display this buffer COUNT forward (in the same ordering as
`other-window'), skipping windows dedicated to their current
buffers, and display in this window the buffer that was displayed
there.

If the optional parameter RIDE is supplied, \"ride\" the buffer,
making its new window the selected one.

As a special case, if COUNT is zero, treat COUNT as 1 and RIDE as
t.  This allows the RIDE parameter to be used interactively.

\(fn &optional COUNT RIDE)" t nil)

(autoload 'aph/swap-buffer-backward "aph-framewin" "\
As `aph/swap-buffer-forward' with direction reversed.

\(fn &optional COUNT RIDE)" t nil)

(autoload 'aph/swap-buffer-forward-and-ride "aph-framewin" "\
As `aph/swap-buffer-forward' but always ride.

\(fn &optional COUNT)" t nil)

(autoload 'aph/swap-buffer-backward-and-ride "aph-framewin" "\
As `aph/swap-buffer-backward' but always ride.

\(fn &optional COUNT)" t nil)

(autoload 'aph/pull-buffer-backward "aph-framewin" "\
Pull buffer from another window.

Display in this buffer the one currently displayed in the window
COUNT windows forward (in the same ordering as `other-window'),
skipping windows dedicated to their current buffers.  Display in
the window previously occupied by this buffer the previous buffer
displayed in that window (using `switch-to-prev-buffer').

\(fn &optional COUNT)" t nil)

(autoload 'aph/pull-buffer-forward "aph-framewin" "\
As `aph/pull-buffer-backward' with direction reversed.

\(fn &optional COUNT)" t nil)

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

;;;### (autoloads nil "aph-info" "aph-info.el" (22028 30124 325364
;;;;;;  500000))
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

;;;### (autoloads nil "aph-org" "aph-org.el" (22060 17701 0 0))
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

\(fn &optional ARG ORG-KEYS RESTRICTION)" t nil)

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

;;;### (autoloads nil "aph-simple" "aph-simple.el" (22083 56965 435260
;;;;;;  400000))
;;; Generated autoloads from aph-simple.el

(autoload 'aph/truncate-lines-on "aph-simple" "\
Cause current buffer to truncate long lines.

\(fn)" nil nil)

(autoload 'aph/truncate-lines-off "aph-simple" "\
Cause current buffer to fold long lines.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "aph-theme" "aph-theme.el" (22055 60847 642136
;;;;;;  900000))
;;; Generated autoloads from aph-theme.el

(autoload 'aph/theme-cycle "aph-theme" "\
Cycle between the themes in `aph/theme-list'.
If none of these themes is currently active, instead load the
first element of `aph/theme-list'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "aph-web" "aph-web.el" (22071 58050 230079
;;;;;;  700000))
;;; Generated autoloads from aph-web.el

(autoload 'aph/browse-url-prefer-eww "aph-web" "\
Browse URL in `eww', or in an external browser.

If a prefix argument is supplied, browse URL in an external
browser; otherwise, use `eww'.

Interactively, prompt the user for URL, using any URL at point as
a default.

\(fn EXTERNAL URL &rest ARGS)" t nil)

(autoload 'aph/browse-url-prefer-external "aph-web" "\
Browse URL in an external browser, or in `eww'.

If a prefix argument is supplied, browse URL in `eww'; otherwise,
use an external browser.

Interactively, prompt the user for URL, using any URL at point as
a default.

\(fn EWW URL &rest ARGS)" t nil)

;;;***

;;;### (autoloads nil nil ("aph-advice.el" "aph-comparators.el" "aph-hooks.el"
;;;;;;  "aph-keys.el" "aph-lib.el" "aph-org-agenda.el" "aph-require.el"
;;;;;;  "aph-shr.el" "init-ahk.el" "init-color-identifiers.el" "init-completion.el"
;;;;;;  "init-core.el" "init-docview.el" "init-draft.el" "init-elfeed.el"
;;;;;;  "init-helm.el" "init-ido.el" "init-keys.el" "init-latex.el"
;;;;;;  "init-lisp-clojure.el" "init-lisp-emacs.el" "init-lisp.el"
;;;;;;  "init-misc.el" "init-org-agenda.el" "init-org-capture.el"
;;;;;;  "init-org.el" "init-package.el" "init-smartparens.el" "init-startup.el"
;;;;;;  "init-visible-mark.el" "init-web.el") (22083 56993 353812
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
