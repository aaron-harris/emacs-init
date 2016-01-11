;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; IIMAGE EXTENSIONS
;;;;============================================================================

;; Extensions for `iimage' module.
(require 'iimage)


;;; Refresh Inline Images
;;;======================
;; Taken from http://vwood.github.io/emacs-images-in-buffer.html
(defun aph/iimage-refresh ()
  "Refresh inline images in current buffer."
  (interactive)
  (clear-image-cache nil)
  (iimage-mode nil)
  (iimage-mode t))


(provide 'aph-iimage)
