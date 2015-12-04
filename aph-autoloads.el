;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aph-mpc" "aph-mpc.el" (22112 53514 667623
;;;;;;  800000))
;;; Generated autoloads from aph-mpc.el

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

;;;### (autoloads nil nil ("aph-advice.el" "aph-comparators.el" "aph-elfeed.el"
;;;;;;  "aph-files.el" "aph-font-lock.el" "aph-framewin.el" "aph-hooks.el"
;;;;;;  "aph-ielm.el" "aph-info.el" "aph-keypad.el" "aph-keys.el"
;;;;;;  "aph-latex.el" "aph-lib.el" "aph-mode-tag.el" "aph-org-agenda.el"
;;;;;;  "aph-org-capture.el" "aph-rect.el" "aph-require.el" "aph-shr.el"
;;;;;;  "aph-symbol.el" "aph-w32.el" "init-core.el" "init-draft.el"
;;;;;;  "init-elfeed.el" "init-ido.el" "init-org-agenda.el" "init-org-capture.el"
;;;;;;  "init-org.el" "init-package.el") (22114 5206 351439 700000))

;;;***

(provide 'aph-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aph-autoloads.el ends here
