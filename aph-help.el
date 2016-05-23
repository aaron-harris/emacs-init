;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HELP EXTENSIONS
;;;;============================================================================

;; Extensions for `help' module.
(require 'help)


;;; No-Confirmation Revert
;;;=======================
(defvar aph/help-mode-confirm-reversion t
  "Whether to confirm before reverting a help buffer.
Enforced by `aph/help-mode--confirm-reversion-advice'")

(defun aph/help-mode--confirm-reversion-advice (args)
  "Advice enforcing `aph/help-mode-confirm-reversion'.
This is intended as :filter-args advice for
`help-mode-revert-buffer'.

With this advice, `help-mode-revert-buffer' will invert its
CONFIRM argument when `aph/help-mode-confirm-reversion' is nil;
this will invert the action of a prefix argument on
`revert-buffer' in `help-mode' buffers, so that confirmation is
only requested if a prefix argument is supplied.

If `aph/help-mode-confirm-reversion' is non-nil (the default),
the behavior of `help-mode-revert-buffer' is unchanged."
  (let ((_ignore-auto   (nth 0 args))
        (maybe-confirm  (nth 1 args))) 
    `(,_ignore-auto ,(if aph/help-mode-confirm-reversion
                         maybe-confirm
                       (not maybe-confirm)))))

(advice-add 'help-mode-revert-buffer
            :filter-args #'aph/help-mode--confirm-reversion-advice)

(provide 'aph-help)
