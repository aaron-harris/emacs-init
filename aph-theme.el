;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; CUSTOM THEME
;;;;============================================================================

(deftheme aph "Personal theme of Aaron Harris")

;; We're extending the zenburn theme, so start by loading that.
(load-theme 'zenburn :noconfirm)

;;; Personal modifications
;;;=======================
(custom-theme-set-faces 'aph '(region ((t (:inverse-video t)))))

(provide-theme 'aph)
