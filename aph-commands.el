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
(defun aph/kill-active-buffer (&optional choose)
  "Kill the active buffer.

With a prefix argument, choose the buffer to kill (as the
standard `kill-buffer')."
  (interactive "P")
  (if choose
      (call-interactively #'kill-buffer)
    (kill-buffer)))

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

;;;###autoload
(defun aph/other-window-backwards (count &optional all-frames)
  "As `other-window' but reversed."
  (interactive "p")
  (other-window (- count) all-frames)) 

;;;###autoload
(defun aph/yank-rectangle-from-kill-ring (&optional arg verbose)
  "Yank the top of kill ring as a rectangle.
Make the \"last killed rectangle\" be the top entry of the kill
ring, then yank that rectangle at point.

With \\[universal-argument] as argument, just save the top entry
of the kill ring as a rectangle, without yanking.  Print a
message to that effect.  When called from elisp, this message is
suppressed unless the optional argument VERBOSE is supplied.
 
With argument N, save the Nth most recent kill instead of the
most recent."
  (interactive "P\np")
  (let ((n          (if (numberp arg) arg 1))
        (save-only  (and arg (listp arg)))
        (width      0))
    (with-temp-buffer
      (yank n)
      ;; Scan for maximum line width
      (dotimes (i (point-max))
        (goto-char (1+ i))
        (setq width (max width (current-column))))
      ;; Pad final line to max width
      (while (< (current-column) width)
        (insert " ")) 
      (copy-rectangle-as-kill 1 (point-max)))
    (if save-only
        (when verbose (message "Most recent kill saved as rectangle."))
      (yank-rectangle))))


;;; Quit help windows
;;;==================

(defvar aph/help-window-names
  '(
    ;; Ubiquitous help buffers
    "*Help*"
    "*Apropos*"
    "*Messages*"
    "*Completions*"
    ;; Other general buffers
    "*Command History*"
    "*Compile-Log*"
    "*disabled command*")
  "Names of buffers that `aph/quit-help-windows' should quit.")

;;;###autoload
(defun aph/quit-help-windows (&optional kill frame)
  "Quit all windows with help-like buffers.

Call `quit-windows-on' for every buffer named in
`aph/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t)))
    (dolist (buffer aph/help-window-names)
      (ignore-errors
        (quit-windows-on buffer kill frame)))))


;;; Sum parenthesized numbers
;;;==========================

(defun aph/sum-parens (string)
  "Sum all parenthesized numbers in STRING."
  ;; The regexp command here removes everything except the numbers to sum.
  (->> (replace-regexp-in-string "[^()]*(\\([0-9]*\\))" "\\1 " string)
       split-string
       (mapcar #'string-to-int)
       (apply #'+)))

;;;###autoload
(defun aph/sum-parens-in-region (start end)
  "Sum all parenthesized numbers in region and echo the result.
If the region is not active, sum all parenthesized numbers in
active buffer.

See `aph/sum-parens' to get similar functionality from elisp."
  (interactive "r")
  (let ((start (if (use-region-p) start (point-min)))
        (end   (if (use-region-p) end   (point-max))))
    (message "Sum of parenthesized numbers in %s: %d"
             (if (use-region-p) "region" "buffer")
             (aph/sum-parens (buffer-substring start end)))))

(provide 'aph-commands)
