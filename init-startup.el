;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; STARTUP SCREEN CONFIGURATION
;;;;============================================================================ 


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
;; Set the default day theme.  This is after machine-specific settings
;; so specific machines can override `aph/theme-day'.
(when (aph/require-softly 'aph-theme)
  (load-theme aph/theme-day :noconfirm))

(provide 'init-startup)
