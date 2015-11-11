;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; WEB BROWSING CONFIGURATION
;;;;============================================================================

;; Configuration for all web browsing features, including `eww' and
;; other packages relying on `shr' for HTML rendering.


;;; Link Displey
;;;=============
(use-package shr
  :defer t
  :config
  (use-package aph-shr)
  (advice-add #'shr-urlify :before #'aph/shr-urlify-advice))


(provide 'init-web)
