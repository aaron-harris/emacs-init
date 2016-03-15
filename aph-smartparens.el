;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SMARTPARENS EXTENSIONS
;;;;============================================================================

;; Extensions for `smartparens' package.
(require 'smartparens)


;;; String Protection
;;;==================
(defun aph/sp-kill-sentence (&optional arg)
  "As `kill-sentence', but don't kill past end of current context.

In a string or comment, kill either to the end of sentence (as
`kill-sentence') or to the end of the string or comment,
whichever is nearer.  Do not kill a closing string or comment
delimiter.  Treat ARG in the same way as `kill-sentence'.

Outside of strings and comments, this should generally behave as
`kill-sentence', but no guarantees are made."
  (interactive "p")
  (let* ((arg              (or arg 1))
         (context          (sp--get-context))
         (compare          (if (< arg 0) #'> #'<))
         (end-of-sentence  (save-excursion (forward-sentence arg))))
    (kill-region (point)
                 (progn
                   (while (and (funcall compare (point) end-of-sentence)
                               (eq (sp--get-context) context))
                     (forward-char (sp--signum arg)))
                   (unless (eq (sp--get-context) context)
                     (backward-char (sp--signum arg)))
                   (point)))))


(provide 'aph-smartparens)
