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

;;; Init Submodules
;;;================
;; The bootstrapping file should have set aph/init-path (the path to the
;; directory containing this file, and our other initialization files).
(add-to-list 'load-path (expand-file-name aph/init-path))

(load "fun-init.el")                    ; General-use functions.

(add-to-list 'load-path (expand-file-name (concat aph/init-path "/org")))
(load "org-init.el")                    ; Org-Mode bootstrapper.

(load "lisp-init.el")                   ; Working with lisp code.
(load "latex-init.el")                  ; Working in LaTeX.
(load "gnus-init.el")                   ; Gnus setup.
(load "keys-init.el")                   ; Setting keybindings.

;;; Miscellaneous Settings
;;;=======================
(prefer-coding-system 'utf-8-unix)        ; Use Unix-style line endings.
(setq-default cursor-type 'box)           ; Use box-style cursor.
(column-number-mode 1)                    ; Show col number in mode line.
(winner-mode 1)                           ; Enable window config undo/redo.
(setq ring-bell-function (lambda () nil)) ; Disable the bell.
(setq inhibit-startup-screen t)           ; Disable the startup screen.
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Start maximized.

;; Auto-fill mode
(setq-default fill-column 80)             ; Set default auto-fill width.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Location settings
(setq calendar-longitude -93.2)
(setq calendar-latitude 45.0)

;;; Custom Theme Settings
;;;======================
(load "wombat-flux-theme.el")
