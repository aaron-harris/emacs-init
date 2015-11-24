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


;;; Display Settings
;;;=================
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

;; Source Variables
(setq-default cursor-type                     'box
              indicate-buffer-boundaries      'right
              resize-mini-windows             t
              ring-bell-function              #'ignore
              scroll-conservatively           1000
              scroll-margin                   1
              scroll-preserve-screen-position :always)
(put 'downcase-region 'disabled nil) 
(put 'upcase-region   'disabled nil)

;; Major Features
(use-package smart-tab
  :ensure t
  :diminish smart-tab-mode
  :config
  (global-smart-tab-mode 1)
  (setq smart-tab-using-hippie-expand        t
        smart-tab-completion-functions-alist nil)
  ;; Use `hippie-expand' in elisp buffers.
  (setq smart-tab-completion-functions-alist
        (assq-delete-all 'emacs-lisp-mode
                         smart-tab-completion-functions-alist)))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (add-hook 'lisp-tag-hook #'company-mode)
  :config
  (setq company-idle-delay nil))

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
  ;; Backup file location
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups"))))
  ;; Make Emacs source read-only
  (when (eq aph/machine 'mpc)
    (setq aph/emacs-source-dirs
          '("C:/Program Files (Portable)/Emacs/share/emacs")))
  (aph/emacs-source-make-read-only))

(use-package simple
  :config
  ;; Global minor modes
  (column-number-mode)
  ;; Misc. settings
  (setq save-interprogram-paste-before-kill t
        shift-select-mode                   nil)
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
  :diminish color-identifiers-mode
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
  :diminish eldoc-mode
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

(use-package xahk-mode
  :mode "\\.ahk\\'")

(use-package server
  :config
  (unless (server-running-p) (server-start))
  (setq server-window 'pop-to-buffer))

(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode 1))

(use-package visible-mark
  :ensure t
  :init
  (defface visible-mark-active
    '((t (:underline "magenta")))
    "Face for the active mark.  Preempts default definition."
    :group 'visible-mark) 
  :config
  (global-visible-mark-mode 1)
  (defface aph/visible-mark-top
    '((t (:underline "light salmon")))
    "Face for the most recent inactive mark."
    :group 'visible-mark)

  (defface aph/visible-mark-other
    '((t (:underline "light goldenrod")))
    "Face for marks other than the most recent."
    :group 'visible-mark)
  (setq visible-mark-max   2
        visible-mark-faces '(aph/visible-mark-top
                             aph/visible-mark-other)))

(use-package paren
  :config
  (show-paren-mode))

(use-package winner
  :config
  (winner-mode))

(use-package help
  :defer t
  :config
  (use-package help+
    :ensure t))

(use-package help-mode
  :defer t
  :config
  (use-package help-mode+
    :ensure t))

(use-package help-fns
  :defer t
  :config
  (use-package help-fns+
    :ensure t 
    :config
    ;; Add Org manual to list of manuals to include links for in help.
    ;; This is a stopgap measure.  In the long run, keying in to
    ;; `Info-file-list-for-emacs' is probably the way to go.
    (setq help-cross-reference-manuals '(("emacs" "elisp" "org")))))

(use-package info
  :defer t
  :config
  ;; Improves functionality of `C-h F' for Org commands.
  (add-to-list 'Info-file-list-for-emacs "org"))

(use-package mule-cmds
  :defer t
  :config
  (prefer-coding-system 'utf-8-unix))

(use-package indent
  :defer t
  :config
  (setq-default indent-tabs-mode nil))

(use-package avy
  :ensure t
  :defer t
  :config
  (setq avy-all-windows     nil
        avy-background      t
        avy-highlight-first t
        avy-style           'pre))

(use-package solar
  :defer t
  :config
  (setq calendar-longitude -93.2
        calendar-latitude   45.0))

(use-package minibuffer
  :defer t
  :config
  (setq completion-auto-help 'lazy))

(use-package saveplace
  :config
  (setq-default save-place t) 
  (setq save-place-file (concat user-emacs-directory "places"))) 

(use-package tooltip
  :config
  (tooltip-mode -1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package page
  :defer t
  :config
  (put 'narrow-to-page  'disabled nil))

(use-package rect
  :defer t
  :config
  (use-package aph-rect)
  (when (>= emacs-major-version 25)
    (advice-add #'rectangle--*-char :around #'aph/rectangle-repetition-fix)))


;; Other 
(aph/require-softly 'init-keys)
(aph/require-softly 'init-startup)

;; Remember to sort out aph-geog.el. Need to do this at work.

(message "Initialization complete!")
(message "-----------------------------------")

(provide 'init-core)
(provide 'init)
