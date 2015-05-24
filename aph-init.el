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

;;; Global Modes
;;;=============
(smartparens-global-mode t)             ; Enable smartparens...
(require 'smartparens-config)           ; ... and its default configuration.
(winner-mode 1)                         ; Enable window config undo/redo.

;;; Auto-fill mode
;;;===============
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Use auto-fill in all text modes.
(setq-default fill-column 80)                 ; Set default auto-fill width.

;;; Miscellaneous Settings
;;;=======================
(prefer-coding-system 'utf-8-unix)        ; Use Unix-style line endings.
(setq-default cursor-type 'box)           ; Use box-style cursor.
(column-number-mode 1)                    ; Show col number in mode line.
(setq ring-bell-function (lambda () nil)) ; Disable the bell.
(setq inhibit-startup-screen t)           ; Disable the startup screen.
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Start maximized.

;; Location settings
(setq calendar-longitude -93.2)
(setq calendar-latitude 45.0)

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

