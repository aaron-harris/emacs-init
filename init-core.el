;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; CORE FILE
;;;;============================================================================

(message "Beginning initialization process...") 


;;; Bootstrapping Variables
;;;========================
;; These variables should have been set by the bootstrapper.
(defvar aph/init-path "~/sync/emacs-init"
  "The path to the directory containing my init files.")

(defvar aph/machine 'default
  "A symbol denoting the specific PC being used.")


;;; Disabling Window Chrome
;;;========================
;; Placing this close to the beginning of initialization should
;; prevent chrome from being drawn at all, rather than drawing it then
;; removing it.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;; Path Management
;;;================
(add-to-list 'load-path (expand-file-name aph/init-path))
(add-to-list 'load-path "~/.emacs.d/lisp")


;;; Loading Submodules
;;;===================
(require 'aph-require)                  ; For `aph/require-softly', etc.
(require 'init-autoloads)
(aph/require-softly 'init-package)

;; Major Features
(aph/require-only-for-machine 'peregrine 'init-gnus)
(aph/require-softly 'init-ido)
(aph/require-softly 'init-org)
(aph/require-softly 'init-smartparens)

;; Specific Modes
(aph/require-softly 'init-docview)
(aph/require-softly 'init-latex)
(aph/require-softly 'init-lisp)
(aph/require-softly 'init-ahk)

;; Other
(aph/require-softly 'init-misc) 
(aph/require-softly 'init-keys)
(aph/require-softly 'init-startup)

;; Remember to sort out aph-geog.el. Need to do this at work.

(message "Initialization complete!")
(message "-----------------------------------")

(provide 'init-core)
(provide 'init)
