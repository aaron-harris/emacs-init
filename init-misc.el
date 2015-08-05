;;;; The Emacs init files of Aaron Harris:
;;;; MISCELLANEOUS SETTINGS
;;;;============================================================================


;;; Emacs Client Settings
;;;======================
(require 'server)
(unless (server-running-p) (server-start)) ; Run Emacs in server mode.
(setq server-window 'pop-to-buffer)     ; Client buffers open in other window.


;;; Disabling Window Chrome
;;;======================== 
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;; Global Modes
;;;============= 
;; Ido Mode (this is from better-defaults.el) 
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-everywhere 1)

;; Visible Mark Mode
(require 'init-visible-mark)

;; Other modes
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

;; Calendar settings
(setq calendar-longitude -93.2
      calendar-latitude 45.0)

;; Clipboard settings (copied verbatim from better-defaults.el)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t) 

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

;; Startup Settings
(setq inhibit-startup-screen t)         ; Disable the splash screen.
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Start maximized.

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
