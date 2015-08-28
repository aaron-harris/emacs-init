;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; MISCELLANEOUS SETTINGS
;;;;============================================================================


;;; Emacs Client Settings
;;;======================
(require 'server)
(unless (server-running-p) (server-start)) ; Run Emacs in server mode.
(setq server-window 'pop-to-buffer)     ; Client buffers open in other window.


;;; Global Modes
;;;=============
;; Smartscan Mode
(when (fboundp 'smartscan-mode)
  (global-smartscan-mode 1))            ; Search for matching symbols.

;; Visible Mark Mode
(aph/require-softly 'init-visible-mark)

;; Smart Tab Mode
(aph/require-softly 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)

;; Built-in modes
(column-number-mode t)                  ; Show col number in mode line.
(show-paren-mode 1)                     ; Highlight matching parens.
(winner-mode 1)                         ; Window config undo and redo. 


;;; Miscellaneous Settings
;;;======================= 
(prefer-coding-system 'utf-8-unix)      ; Use Unix-style line endings.
(setq-default indent-tabs-mode nil)     ; Don't use hard tabs.
(setq-default cursor-type 'box)         ; Use box-style cursor.
(setq ring-bell-function #'ignore)      ; Disable the bell.
(setq apropos-do-all t)                 ; More thorough apropos searches.
(setq-default indicate-buffer-boundaries 'right) ; Show buffer start/end.
(setq resize-mini-windows t)            ; Allow minibuffer to shrink.

;; Calendar settings
(setq calendar-longitude -93.2
      calendar-latitude 45.0)

;; Clipboard settings (copied verbatim from better-defaults.el)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Completion settings
(setq completion-auto-help 'lazy)

;; Saved place and backup settings
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Scrolling settings
(setq scroll-margin 1 
      scroll-conservatively 1000
      scroll-preserve-screen-position :always)

;; Tooltip Settings
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Uniquify settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;; Enabling Commands
;;;==================
;; These enable commands which are by default disabled.
(put 'narrow-to-page 'disabled nil) 

(provide 'init-misc)
