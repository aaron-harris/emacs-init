;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; LISP-MODE EXTENSIONS
;;;;============================================================================

;; Extensions for `lisp-mode' module.
(require 'lisp-mode)


;;; Evaluation Commands
;;;====================
(defun aph/eval-region-or-buffer ()
  "As `eval-region', or `eval-buffer' if region inactive."
  (interactive)
  (call-interactively
   (if (use-region-p) #'eval-region #'eval-buffer)))


(provide 'aph-lisp-mode)
