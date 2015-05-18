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

;;; General Settings
;;;=================
(prefer-coding-system 'utf-8-unix) ; Use Unix-style line endings.
(setq-default cursor-type 'box) ; Use box-style cursor.
(column-number-mode 1) ; Enable column-number-mode everywhere.
(winner-mode 1) ; Use winner-mode for window config undo/redo.
(setq-default fill-column 80) ; Set the default auto-fill width to 80.
(setq ring-bell-function (lambda () nil)) ; Disable the bell.
(setq inhibit-startup-screen t) ; Disable the startup screen.
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Start maximized.

;;; Settings for emacsclient usage
;;;===============================
(server-start) ; Run Emacs in server mode.
(setq server-window 'pop-to-buffer) ; emacsclient buffers open in other window.

;; Setting location in Calendar.
(setq calendar-longitude -93.2)
(setq calendar-latitude 45.0)

;;; Init Submodules
;;;================
;; The bootstrapping file should have set aph/init-path (the path to the
;; directory containing this file, and our other initialization files).
(add-to-list 'load-path (expand-file-name aph/init-path))

(load "fun-init.el")

;; Org-Mode initialization is so complex, it uses its own directory, which we
;; also have to add to the load path.
(add-to-list 'load-path (expand-file-name (concat aph/init-path "/org")))
(load "org-init.el")

(load "cider-init.el")
(load "auctex-init.el")
(load "gnus-init.el")
(load "keys-init.el")
(load "modes-init.el")

;;; Custom Theme Settings
;;;======================
(load "wombat-flux-theme.el")
