;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SHELL INTERACTION FUNCTIONS
;;;;============================================================================ 


;;; Mercurial Commands
;;;===================

;;;###autoload
(defun aph/hg-commit (&optional flags)
  "Run the shell command \"hg commit\" asynchronously.
With a prefix argument, prompt for additional flags."
  (interactive
   (list (if current-prefix-arg
             (read-from-minibuffer "Flags for hg commit: ")
           "")))
  (async-shell-command (format  "hg commit %s" flags)))

;;;###autoload
(defun aph/hg-log (&optional n)
  "Run the shell command \"hg log\".
If an argument N is provided, instead run \"hg log -l N\"."
  (interactive "p")
  (shell-command (concat "hg log" (if n (format " -l %d" n) ""))))

;;;###autoload
(defun aph/hg-status ()
  "Run the shell command \"hg status\"."
  (interactive)
  (shell-command "hg status"))

(provide 'aph-shell)
