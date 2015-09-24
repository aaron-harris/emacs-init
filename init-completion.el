;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; COMPLETION SETTINGS
;;;;============================================================================


;;; Smart Tab
;;;==========
(when (aph/require-softly 'smart-tab)
  (global-smart-tab-mode 1)

  ;; Always use `hippie-expand'.  In situations where we want to use a
  ;; mode-specific completion function, I'd rather configure
  ;; `hippie-expand' to use it and let `smart-tab' just call
  ;; `hippie-expand' in all cases.
  (setq smart-tab-using-hippie-expand t)
  (setq smart-tab-completion-functions-alist nil))
