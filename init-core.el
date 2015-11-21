;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; CORE FILE
;;;;============================================================================


;;; Bootstrapping Variables
;;;========================
;; These variables should have been set by the bootstrapper.
(defvar aph/init-path "~/sync/emacs/init"
  "The path to the directory containing my init files.")

(defvar aph/machine 'default
  "A symbol denoting the specific PC being used.")


;;; Disabling Window Chrome
;;;========================
;; Placing this close to the beginning of initialization should
;; prevent chrome from being drawn at all, rather than drawing it then
;; removing it.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;; Path Management
;;;================
(add-to-list 'load-path (expand-file-name aph/init-path))


;;; Loading Submodules
;;;===================
(require 'aph-require)                  ; For `aph/require-softly', etc.
(aph/require-softly 'aph-autoloads)
(aph/require-softly 'init-package)

;; Major Features
(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode 1) 
  (setq smart-tab-using-hippie-expand        t
        smart-tab-completion-functions-alist nil)
  ;; Use `hippie-expand' in elisp buffers.
  (setq smart-tab-completion-functions-alist
        (assq-delete-all 'emacs-lisp-mode
                         smart-tab-completion-functions-alist))
  :diminish smart-tab-mode)

(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'lisp-tag-hook #'company-mode)
  :config
  (setq company-idle-delay nil)
  :diminish company-mode)

(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq helm-scroll-amount                    8  ; For `C-M-v', `C-S-M-v'
        helm-split-window-in-side-p           t  ; Leave window splits alone
        helm-ff-file-name-history-use-recentf t  ; Better file history
        helm-ff-search-library-in-sexp        t) ; Like `ffap' for `require'
  ;; Info pages to use for `helm-info-at-point'.
  (setq helm-info-default-sources
        '(helm-source-info-emacs
          helm-source-info-elisp
          helm-source-info-cl
          helm-source-info-eieio
          helm-source-info-org
          helm-source-info-pages))
  ;; Turn Helm off for Org-mode refiling, since Helm can't handle
  ;; multiple levels of refile targets.
  (add-to-list 'helm-completing-read-handlers-alist
               '(org-refile . nil)))

(use-package helm-descbinds
  :requires helm
  :ensure t
  :config (helm-descbinds-mode))

(use-package projectile
  :requires helm
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system     'helm
        projectile-switch-project-action #'helm-projectile
        projectile-enable-caching        t)
  (helm-projectile-on))

(use-package shr
  :defer t
  :config
  (use-package aph-shr)
  (advice-add #'shr-urlify :before #'aph/shr-urlify-advice))

(use-package org
  :ensure t
  :defer t
  :config
  (use-package init-org))
(require 'org)                          ; Temporary pending refactoring

(use-package org-mobile
  :disabled t
  :config
  (setq org-mobile-directory       "~/sync/mobile"
      org-mobile-inbox-for-pull (concat org-directory "/capture.org"))
  (when (eq aph/machine 'mpc)
    (setq org-mobile-checksum-binary
          "C:/Program Files (Portable)/GnuWin Core Utilities/bin/sha1sum.exe")))

(use-package smartparens-config
  :ensure smartparens
  :init
  (add-hook 'lisp-tag-hook #'smartparens-strict-mode)
  :config 
  (smartparens-global-mode)
  ;; String handling
  (add-to-list 'sp-navigate-consider-stringlike-sexp 'org-mode)
  (add-to-list 'sp-navigate-consider-stringlike-sexp 'lisp-mode)
  (add-to-list 'sp-navigate-consider-stringlike-sexp 'clojure-mode)
  ;; Disable '' pair in minibuffer for `eval-expression'
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  ;; Use `` instead of `' in Clojure mode
  (sp-local-pair 'clojure-mode "`" "`"
                 :when '(sp-in-string-p sp-in-comment-p)))

(use-package elfeed
  :ensure t
  :defer t
  :config
  (use-package aph-elfeed)
  (setq elfeed-sort-order            'ascending
        aph/elfeed-favorite-filters  '("@6-months-ago +unread" "+todo"))
  (setq url-queue-parallel-processes 1
        url-queue-timeout            30)
  (setq elfeed-db-directory "~/sync/elfeed")
  (use-package init-elfeed))            ; Feed list

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 200)
  ;; On mpc, Ghostview has a different name.
  (when (eq aph/machine 'mpc)
    (setq doc-view-ghostscript-program "mgs.exe")))

(use-package tex-site
  :ensure auctex
  :defer t
  :config
  (use-package aph-latex)
  (setq-default TeX-master nil)
  ;; Caching settings
  (setq TeX-auto-save  t
        TeX-parse-self t)
  ;; Outline settings
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  ;; Preview settings
  (setq preview-image-type          'dvipng
        preview-preserve-counters   t
        preview-auto-cache-preamble t)
  ;; Math mode settings
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (setq LaTeX-math-list
        '(("'" (lambda () (interactive) (insert "^{\\prime}")))))
  ;; RefTeX settings
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (setq reftex-plug-into-auctex t)
  ;; Compilation and viewer settings
  (setq TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook #'aph/LaTeX-use-emacs-as-viewer)
  ;; Miscellaneous settings
  (with-eval-after-load 'aph-commands
    (add-to-list 'aph/help-window-names "*TeX Help*"))
  (defun aph/LaTeX-mode-hook ()
    "Apply my settings for `LaTeX-mode'."
    (setq fill-column 75))
  (add-hook 'LaTeX-mode-hook #'aph/LaTeX-mode-hook))

(use-package aph-mode-tag)

(aph/mode-tag-create 'lisp
    "Tag for modes used to edit any sort of Lisp, including REPLs.")

;; All-Lisp config 
(use-package lisp-mode
  :defer t 
  :config
  ;; Mode tags
  (aph/mode-tag-add 'lisp-mode             'lisp)
  (aph/mode-tag-add 'emacs-lisp-mode       'lisp)
  (aph/mode-tag-add 'lisp-interaction-mode 'lisp))

(use-package files
  :defer t
  :config
  (use-package aph-files)
  (when (eq aph/machine 'mpc)
    (setq aph/emacs-source-dirs
          '("C:/Program Files (Portable)/Emacs/share/emacs")))
  (aph/emacs-source-make-read-only))

(use-package simple
  :defer t
  :config
  ;; Tweaking `eval-expression'
  (defun aph/mode-tag-run-hook--lisp ()
    "Run the hook for the `lisp' mode tag."
    (run-hooks 'lisp-tag-hook))
  (add-hook 'eval-expression-minibuffer-setup-hook
            #'aph/mode-tag-run-hook--lisp) 
  (setq eval-expression-print-length nil
        eval-expression-print-level  nil)
  (when (eq aph/machine 'mpc)
    (setq aph/eval-expression-clean-output t)))

;; Not entirely sure where this should go yet.
(defun aph/hippie-expand-config-lisp ()
  "Configure `hippie-expand-try-functions-list' for Lisps."
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-list
          try-expand-line
          try-expand-line-all-buffers)))
(add-hook 'lisp-tag-hook #'aph/hippie-expand-config-lisp)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'lisp-tag-hook #'rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :ensure t
  :defer t
  :init
  (add-hook 'lisp-tag-hook #'color-identifiers-mode)
  :config 
  (setq color-identifiers:num-colors      12
        color-identifiers:color-luminance 0.65)

  (use-package aph-font-lock)
  (add-hook 'color-identifiers-mode-hook #'aph/font-lock-decolorize)
  
  ;; In the function `color-identifiers:clojure-declarations-in-sexp',
  ;; there is a call to `evenp', which is not defined; presumably, the
  ;; package maintainers are expecting use to be using `cl' rather
  ;; than `cl-lib'.  As a stopgap, I'm aliasing just `evenp' globally.
  (defalias #'evenp #'cl-evenp))

(use-package clojure-mode
  :ensure t
  :defer t
  :init
  (aph/mode-tag-create 'clojure
    "Tag for modes used to edit Clojure, including REPLs.")
  :config
  (aph/mode-tag-add 'clojure-mode 'lisp)
  (aph/mode-tag-add 'clojure-mode 'clojure)
  (add-hook 'clojure-tag-hook #'subword-mode))

(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-auto-select-error-buffer      nil
        cider-show-error-buffer             'except-in-repl
        cider-repl-pop-to-buffer-on-connect nil)
  (aph/mode-tag-add 'cider-repl-mode 'lisp)
  (aph/mode-tag-add 'cider-repl-mode 'clojure)
  (eval-after-load 'aph-commands
  '(add-to-list 'aph/help-window-names "*cider-doc*"))
  ;; Output from the JVM has Windows-style newlines, so we need to
  ;; strip those unless we want to see ^M characters in Cider buffers.
  (use-package aph-w32)
  (add-hook 'cider-repl-mode-hook            #'aph/remove-dos-eol)
  (add-hook 'cider-macroexpansion-mode-hook  #'aph/remove-dos-eol)
  (add-hook 'cider-test-report-mode-hook     #'aph/remove-dos-eol))

(use-package eldoc
  :defer t
  :init
  (add-hook 'lisp-tag-hook #'eldoc-mode)) 

(use-package ielm
  :defer t
  :config
  (aph/mode-tag-add 'ielm-mode 'lisp))

(use-package font-lock
  :defer t
  :config
  (eval-after-load 'dash        #'dash-enable-font-lock)
  (eval-after-load 'aph-require #'aph/require-enable-font-lock))

;; Specific Modes
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
