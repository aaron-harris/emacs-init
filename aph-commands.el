;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM COMMANDS
;;;;============================================================================

(require 'dash)                         ; For `->>'


;;; Simple Commands
;;;================
;; Commands in this section are "one-offs" whose functionality is
;; entirely contained in their own `defun'.

;;;###autoload
(defun aph/apropos-function (pattern &optional commands-only var-predicate)
  "As `apropos-command', but show all functions by default.
The behavior of a prefix argument is inverted, so supplying a
prefix argument will show only commands (and override a non-nil
value for `apropos-do-all')"
  (interactive (list (apropos-read-pattern
		      (if current-prefix-arg
			  "command" "command or function"))
		     current-prefix-arg))
  (apropos-command pattern (not commands-only) var-predicate))

;;;###autoload
(defun aph/delete-frame-or-exit (&optional arg)
  "Delete this frame. With only one frame, exit Emacs.

When there is more than one visible frame, run `delete-frame'.
Otherwise, exit Emacs with `save-buffers-kill-terminal' after
confirming this with user.

If a prefix ARG is supplied, ignore it in the multiple-frame
case.  Otherwise, bypass confirmation and pass the argument to
`save-buffers-kill-terminal'."
  (interactive "P")
  (cond
   ((> (length (visible-frame-list)) 1)  (delete-frame))
   ((or arg (y-or-n-p "Quit Emacs?"))    (save-buffers-kill-terminal arg))
   (t                                    (message "Abort"))))

;;;###autoload
(defun aph/kp-enter-newline-toggle (&optional verbose)
  "Toggle whether <kp-enter> should act like C-n instead of enter.
Accomplish this by updating the entry for <kp-enter> in
`function-key-map'.  If this entry is something other than these
two keys, restore it to the default mapping (enter, i.e. C-m).

When called interactively, or when the optional parameter is
supplied, supply feedback messages.  Always print a message when
the entry being overridden is unexpected.

For convenience, return the new mapping."
  (interactive "p")
  (let ((current-def  (lookup-key function-key-map (kbd "<kp-enter>")))
        (new-def      nil)
        (msg      nil))
    ;; The keymap uses [13] for C-m, so we'll respect its conventions
    ;; and use [14] for C-n.
    (cond
     ((equal current-def [13])
      (setq new-def [14]
            msg      "Decoupling <kp-enter> for motion"))
     ((equal current-def [14])
      (setq new-def  [13]
            msg      "Recoupling <kp-enter> to <enter>"))
     (t
      (setq new-def [13]
            verbose  t
            msg      "Restoring <kp-enter> to default (was %S)")))
    
    (define-key function-key-map (kbd "<kp-enter>") new-def)
    (when verbose (message msg current-def))
    new-def))

;;;###autoload
(defun aph/newline (n)
  "As `newline', with support for negative argument.
An argument of -N calls `join-line' N times."
  (interactive "p")
  (if (< n 0)
      (dotimes (i (- n)) (join-line))
    (newline n))) 

(provide 'aph-commands)
