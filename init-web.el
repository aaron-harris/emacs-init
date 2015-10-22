;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; WEB BROWSING CONFIGURATION
;;;;============================================================================

;; Configuration for all web browsing features, including `eww' and
;; other packages relying on `shr' for HTML rendering.


;;; Link Displey
;;;=============
;; Do not underline image links, as this causes problems when using
;; `shr-zoom-image'.
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

(advice-add #'shr-urlify :before #'aph/shr-urlify-advice)


(provide 'init-web)
