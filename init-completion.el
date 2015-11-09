;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; COMPLETION SETTINGS
;;;;============================================================================


;;; Smart Tab
;;;==========
(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode 1)
  ;; Always use `hippie-expand'.  In situations where we want to use a
  ;; mode-specific completion function, I'd rather configure
  ;; `hippie-expand' to use it and let `smart-tab' just call
  ;; `hippie-expand' in all cases.
  (setq smart-tab-using-hippie-expand        t
        smart-tab-completion-functions-alist nil)
  :diminish smart-tab-mode)


;;; Company Mode
;;;=============
;; In certain modes, we want to use `company-mode' instead of
;; `smart-tab'.  We still want to use tab completion and not idle
;; completion.
(use-package company
  :ensure t
  :defer t
  :config
  (setq company-idle-delay nil)
  :diminish company-mode)

(provide 'init-completion)
