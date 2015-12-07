;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; FACE REMAPPING EXTENSIONS
;;;;============================================================================

;; Extensions for `face-remap' module.
(require 'face-remap)


;;; Text Scaling
;;;=============
(defun aph/preserving-text-scale (fn &rest args)
  "Call FN with ARGS, preserving current text scaling."
  (require 'face-remap)                 ; For `text-scale-mode-amount'
  (let ((scale text-scale-mode-amount))
    (apply fn args)
    (text-scale-set scale)))


(provide 'aph-face-remap)
