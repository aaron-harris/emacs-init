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
(defun aph/newline (n)
  "As `newline', with support for negative argument.
An argument of -N calls `join-line' N times."
  (interactive "p")
  (if (< n 0)
      (dotimes (i (- n)) (join-line))
    (newline n))) 

(provide 'aph-commands)
