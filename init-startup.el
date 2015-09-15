;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; STARTUP SCREEN CONFIGURATION
;;;;============================================================================ 

(require 'aph-require)                  ; For `aph/require-softly'
(require 'aph-hooks)                    ; For `aph/add-hook-safely'


;;; Universal Startup Settings
;;;===========================
(setq inhibit-startup-screen t)


;;; Machine-Specific Settings
;;;==========================
;; On Peregrine, the frame should be maximized, and it should contain
;; only one window, with our smart agenda.
(when (eq aph/machine 'peregrine)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (aph/add-hook-safely 'after-init-hook #'aph/org-agenda-display-smart-agenda)
  (add-hook            'after-init-hook #'delete-other-windows))


;;; Theme Settings
;;;===============
(defvar aph/theme-hashes 
  '((aph . "416ef2f2057400db7cab91aeacb583b4b539c549f4713260282a903d79344312"))
  "An alist mapping theme names to their hashes.")

;; Treat 'aph theme as safe.
(add-to-list 'custom-safe-themes (cdr (assq 'aph aph/theme-hashes)))

;; Then load it.
(when (aph/require-softly 'aph-theme)
  (load-theme 'aph :noconfirm))

(provide 'init-startup)
