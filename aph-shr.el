;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SHR EXTENSIONS
;;;;============================================================================

;; Extensions for `shr' package. 


;;; Link Displey
;;;=============
;; Advice so that `shr' does not underline image links.  This is
;; intended to fix display errors associated with `shr-zoom-image'.
(defface aph/shr-link-img
  '((t (:inherit shr-link :underline nil)))
  "Font for image links."
  :group 'shr)

(defun aph/shr-urlify-advice (start url &optional title)
  "Make `shr-urlify' use face `aph/shr-link-img' for image links.
Intended as :before advice for `shr-urlify'."
  (when (get-text-property start 'image-url)
    (aph/advice-once #'shr-add-font :filter-args
                     (lambda (args)
                       (setf (nth 2 args) 'aph/shr-link-img)
                       args))))


(provide 'aph-shr)
