;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; HELM CONFIGURATION
;;;;============================================================================

(require 'helm)
(require 'helm-config)


;;; Basic Settings
;;;===============
(helm-mode 1)
(helm-autoresize-mode t)
(setq helm-split-window-in-side-p t)           ; Leave window splits alone
(setq helm-ff-search-library-in-sexp t)        ; Like `ffap' for `require'
(setq helm-scroll-amount 8)                    ; For `C-M-v', `C-S-M-v'
(setq helm-ff-file-name-history-use-recentf t) ; Better file history


;;; Info Sources
;;;=============
;; Info pages to use for `helm-info-at-point'.
(setq helm-info-default-sources
      '(helm-source-info-emacs
        helm-source-info-elisp
        helm-source-info-cl
        helm-source-info-eieio
        helm-source-info-org
        helm-source-info-pages))


;;; Handler Setup
;;;==============
;; Turn Helm off for Org-mode refiling, since Helm can't handle
;; multiple levels of refile targets.
(add-to-list 'helm-completing-read-handlers-alist
             '(org-refile . nil))


;;; Extensions
;;;===========
(when (aph/require-softly 'helm-descbinds)
  (helm-descbinds-mode))

(when (aph/require-softly 'projectile)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action #'helm-projectile)
  (helm-projectile-on))

(provide 'init-helm)
