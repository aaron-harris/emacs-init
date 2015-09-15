;;; aph-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


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

;;;### (autoloads nil "aph-org" "aph-org.el" (22007 15106 798839
;;;;;;  500000))
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

;;;### (autoloads nil nil ("aph-comparators.el" "aph-framewin.el"
;;;;;;  "aph-functions-dash.el" "aph-functions.el" "aph-hooks.el"
;;;;;;  "aph-keys.el" "aph-mpc.el" "aph-org-agenda.el" "aph-require.el"
;;;;;;  "aph-shell.el" "aph-theme.el" "init-ahk.el" "init-color-identifiers.el"
;;;;;;  "init-core.el" "init-docview.el" "init-draft.el" "init-gnus.el"
;;;;;;  "init-ido.el" "init-keys.el" "init-latex.el" "init-lisp-clojure.el"
;;;;;;  "init-lisp-emacs.el" "init-lisp.el" "init-misc.el" "init-org-agenda.el"
;;;;;;  "init-org-capture.el" "init-org.el" "init-package.el" "init-smartparens.el"
;;;;;;  "init-startup.el" "init-visible-mark.el") (22008 19580 188725
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
