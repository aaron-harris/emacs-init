;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; FONT LOCK EXTENSIONS
;;;;============================================================================

;; Extensions for the `font-lock' package. 


;;; Decolorization
;;;===============
;; In color identifiers mode, we want to reduce other use of color so
;; the identifiers stand out.
(defun aph/font-lock-decolorize ()
    "Changes font lock faces to reduce use of color."
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground nil
                      :weight 'ultra-bold)
  (set-face-attribute 'font-lock-function-name-face nil
                      :weight 'bold)
  (set-face-attribute 'font-lock-variable-name-face nil
                      :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil 
                      :weight 'bold
                      :slant 'italic)
  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-type-face nil
                      :foreground nil
                      :underline t)) 

(provide 'aph-font-lock)
