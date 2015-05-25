;;;; The Emacs init files of Aaron Harris:
;;;; MAIN FILE
;;;;============================================================================

;;; Package Repositories
;;;=====================
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;; Emacs Client Settings
;;;======================
(server-start)                          ; Run Emacs in server mode.
(setq server-window 'pop-to-buffer)     ; Client buffers open in other window.

;;; Disabling Window Chrome
;;;========================
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Global Modes
;;;=============
;; Ido Mode (these is from better-defaults.el)
(ido-mode 1)
(setq ido-enable-flex-matching t)

;; Smartparens (and default configuration)
(smartparens-global-mode 1)
(require 'smartparens-config)

;; Other modes
(column-number-mode t)                  ; Show col number in mode line.
(show-paren-mode 1)                     ; Highlight matching parens.
(winner-mode 1)                         ; Window config undo and redo.

;;; Auto-fill mode
;;;===============
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Use auto-fill in all text modes.
(setq-default fill-column 80)                 ; Set default auto-fill width.

;;; Miscellaneous Settings
;;;=======================
(prefer-coding-system 'utf-8-unix)        ; Use Unix-style line endings.
(setq-default indent-tabs-mode nil)       ; Don't use hard tabs.
(setq-default cursor-type 'box)           ; Use box-style cursor.
(setq ring-bell-function (lambda () nil)) ; Disable the bell.
(setq apropos-do-all t)                   ; More thorough apropos searches.

;; Calendar settings
(setq calendar-longitude -93.2)
(setq calendar-latitude 45.0)

;; Clipboard settings (copied verbatim from better-defaults.el)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; Saved place and backup settings
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Startup Settings
(setq inhibit-startup-screen t)         ; Disable the splash screen.
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Start maximized.

;; Uniquify settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Init Submodules
;;;================
;; The bootstrapping file should have set aph/init-path (the path to the
;; directory containing this file, and our other initialization files).
(add-to-list 'load-path (expand-file-name aph/init-path))

(load "fun-init.el")                    ; General-use functions and variables.

(add-to-list 'load-path (expand-file-name (concat aph/init-path "/org")))
(load "org-init.el")                    ; Org-Mode bootstrapper.

(load "lisp-init.el")                   ; Working with lisp code.
(load "latex-init.el")                  ; Working in LaTeX.
(load "gnus-init.el")                   ; Gnus setup.
(load "keys-init.el")                   ; Setting keybindings.

;;; Custom Theme Settings
;;;======================
(setq custom-safe-themes t)             ; Treat all themes as safe.
(load-theme 'zenburn)

;; This was my old theme, a version of Wombat that I modified to be more
;; compatible with f.lux:

;; (load "wombat-flux-theme.el")

