;;;; The Emacs init file of Aaron Harris.
;;;; COLOR IDENTIFIERS CONFIGURATION
;;;;============================================================================


;;; General Settings
;;;=================
(setq color-identifiers:num-colors 12)
(setq color-identifiers:color-luminance 0.65)


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
                      :underline t))

(add-hook 'color-identifiers-mode-hook #'aph/font-lock-decolorize)


;;; Patching
;;;=========
;; In the function `color-identifiers:clojure-declarations-in-sexp',
;; there is a call to `evenp', which is not defined.  I'm guessing
;; that cl-lib renamed this at some point.  As a stopgap, I'm just
;; restoring the `evenp' alias globally.
(defalias #'evenp #'cl-evenp)

(provide 'init-color-identifiers)
