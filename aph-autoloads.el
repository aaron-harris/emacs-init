;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aph-commands" "aph-commands.el" (22107 42214
;;;;;;  438722 0))
;;; Generated autoloads from aph-commands.el

(autoload 'aph/apropos-function "aph-commands" "\
As `apropos-command', but show all functions by default.
The behavior of a prefix argument is inverted, so supplying a
prefix argument will show only commands (and override a non-nil
value for `apropos-do-all')

\(fn PATTERN &optional COMMANDS-ONLY VAR-PREDICATE)" t nil)

(autoload 'aph/newline "aph-commands" "\
As `newline', with support for negative argument.
An argument of -N calls `join-line' N times.

\(fn N)" t nil)

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

;;;### (autoloads nil "aph-org" "aph-org.el" (22085 11309 492504
;;;;;;  800000))
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

;;;### (autoloads nil "aph-simple" "aph-simple.el" (22111 27527 912419
;;;;;;  700000))
;;; Generated autoloads from aph-simple.el

(autoload 'aph/eval-expression-toggle-clean-output "aph-simple" "\
Toggle the variable `aph/eval-expression-clean-output'.

\(fn)" t nil)

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

;;;### (autoloads nil "aph-window" "aph-window.el" (22107 19482 83804
;;;;;;  400000))
;;; Generated autoloads from aph-window.el

(defvar aph/help-window-names '("*Help*" "*Apropos*" "*Messages*" "*Completions*" "*Command History*" "*Compile-Log*" "*disabled command*") "\
Names of buffers that `aph/quit-help-windows' should quit.")

;;;***

;;;### (autoloads nil nil ("aph-advice.el" "aph-comparators.el" "aph-files.el"
;;;;;;  "aph-font-lock.el" "aph-framewin.el" "aph-hooks.el" "aph-ielm.el"
;;;;;;  "aph-keypad.el" "aph-keys.el" "aph-latex.el" "aph-lib.el"
;;;;;;  "aph-mode-tag.el" "aph-org-agenda.el" "aph-org-capture.el"
;;;;;;  "aph-rect.el" "aph-require.el" "aph-shr.el" "aph-symbol.el"
;;;;;;  "aph-w32.el" "init-core.el" "init-draft.el" "init-elfeed.el"
;;;;;;  "init-ido.el" "init-keys.el" "init-org-agenda.el" "init-org-capture.el"
;;;;;;  "init-org.el" "init-package.el" "init-startup.el") (22111
;;;;;;  27536 906797 800000))

;;;***

(provide 'aph-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aph-autoloads.el ends here
