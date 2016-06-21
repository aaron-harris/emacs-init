;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; WINDOW EXTENSIONS
;;;;============================================================================

;; Extensions to the `window' module.
(require 'hydra)


;;; Extensions to `other-window'
;;;=============================
(defun aph/other-window-backward (count &optional all-frames)
  "As `other-window' but reversed."
  (interactive "p")
  (other-window (- count) all-frames))


;;; Scrolling Commands
;;;===================
(defhydra aph/hydra-scroll-other (:color pink)
  "Scroll other"
  ("C-v" scroll-other-window      "fwd")
  ("M-v" (scroll-other-window '-) "back") 
  ("C-g" nil                      "quit" :color blue))


(provide 'aph-window)
