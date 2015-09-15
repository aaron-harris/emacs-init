;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; CUSTOM THEME
;;;;============================================================================

(require 'dash)                         ; For `-drop-while'

(deftheme aph "Personal theme of Aaron Harris")


;;; Base Theme Setup
;;;=================
;; This theme is designed to be an extension for some other base
;; theme.  Support is provided for cycling the base theme used.

(defvar aph/theme-list '(zenburn hc-zenburn)
  "A list of themes the 'aph theme can use as bases.
The modifications of 'aph occur on top of one of these.  When the
theme is first loaded, the first element is taken as the base.
Subsequently, `aph/theme-cycle' can be used to change out the
base.

Be aware that 'aph will treat all of these themes as safe
regardless of the value of `custom-safe-themes'.")

;; Load all the base themes, and enable the first one.
(load-theme (car aph/theme-list) :noconfirm)
(dolist (thm (cdr aph/theme-list))
        (load-theme thm :noconfirm :noenable))

;;;###autoload
(defun aph/theme-cycle ()
  "Cycle between the themes in `aph/theme-list'.
If none of these themes is currently active, instead load the
first element of `aph/theme-list'."
  (interactive)
  (let ((themes (-drop-while (lambda (thm)
                               (not (custom-theme-enabled-p thm)))
                             aph/theme-list)))
    (if (null themes)
        (enable-theme (car aph/theme-list))
      (disable-theme (car themes))
      (enable-theme (or (cadr themes) (car aph/theme-list)))
      (enable-theme 'aph))))


;;; Theme Settings
;;;===============
(custom-theme-set-faces 'aph '(region ((t (:inverse-video t)))))

(provide-theme 'aph)
