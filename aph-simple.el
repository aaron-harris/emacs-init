;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SIMPLE EXTENSIONS
;;;;============================================================================

;; Extensions for `simple' package.
(require 'simple)

(require 'hydra)


;;; Newline Commands
;;;=================
(defun aph/open-line (n)
  "As `open-line', with support for negative argument.
An argument of -N calls `join-line' with an argument N times."
  (interactive "p")
  (if (< n 0)
      (dotimes (i (- n)) (join-line :invert))
    (open-line n)))


;;; Yank Commands
;;;==============
(defun aph/yank-command-on-string (string command &rest args)
  "Return text inserted by COMMAND when STRING is most recent kill.
Pass any ARGS to COMMAND.

Do this in a temporary buffer, so that current buffer is not
changed or modified.  (Of course, if COMMAND selects a buffer
itself, this guarantee cannot be enforced.)

Presumably COMMAND is a function that yanks text from the kill
ring (possibly with some other processing involved).  This
function effectively transforms that command into a string
processing command, although `kill-ring' is still changed as a
side-effect (by the addition of STRING, plus whatever changes are
made by COMMAND)."
  (with-temp-buffer
    (kill-new string)
    (apply command args)
    (buffer-string)))


;;; Motion Commands
;;;================
(defun aph/move-beginning-of-line (&optional arg)
  "Combine `move-beginning-of-line' and `back-to-indentation'.

Behave as `move-beginning-of-line', unless point is already at
beginning of line, in which case call `back-to-indentation'.

If ARG is supplied, then it is interpreted as in
`move-beginning-of-line' and `back-to-indentation' is not
called.

Return the new value of point."
  (interactive "^P")
  (cond
   (arg     (move-beginning-of-line (prefix-numeric-value arg)))
   ((bolp)  (back-to-indentation))
   (:else   (move-beginning-of-line 1)))
  (point))

(provide 'aph-simple)
