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


;;; Package Loading
;;;================
(add-to-list 'load-path (expand-file-name aph/init-path))

(require 'aph-require)                  ; For `aph/require-softly', etc.
(aph/require-softly 'aph-autoloads)
(aph/require-softly 'init-package)
(use-package aph-mode-tag)


;;; Configuration: Source-Level
;;;===========================
(prefer-coding-system 'utf-8-unix)

;; UI Configuration
(setq-default cursor-type                     'box
              indent-tabs-mode                nil
              indicate-buffer-boundaries      'right
              resize-mini-windows             t
              ring-bell-function              #'ignore
              scroll-conservatively           1000
              scroll-margin                   1
              scroll-preserve-screen-position :always) 

;; Use Windows keys for super modifier.
(setq w32-lwindow-modifier       'super
      w32-pass-lwindow-to-system nil 
      w32-rwindow-modifier       'super
      w32-pass-rwindow-to-system nil)

;; Enable some disabled commands
(put 'downcase-region 'disabled nil) 
(put 'upcase-region   'disabled nil)

;; Keybindings for source-defined commands
(bind-key "C-S-t" 'transpose-paragraphs)


;;; Mode Tags
;;;========== 
(aph/mode-tag-create 'lisp
  "Tag for modes used to edit any sort of Lisp, including REPLs.")


;;; Package Configuration
;;;======================
(use-package avy
  :ensure t
  :defer t
  :config
  (setq avy-all-windows     nil
        avy-background      t
        avy-highlight-first t
        avy-style           'pre))

(use-package bookmark
  ;; Move from "C-x r" prefix to "C-c b"
  :bind (("C-x r b" . nil)
         ("C-x r l" . nil)
         ("C-x r m" . nil)
         ("C-c b b" . bookmark-jump)
         ("C-c b l" . bookmark-bmenu-list)
         ("C-c b m" . bookmark-set)))

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

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (add-hook 'lisp-tag-hook #'company-mode)
  :config
  (setq company-idle-delay nil))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 200)
  ;; On mpc, Ghostview has a different name.
  (when (eq aph/machine 'mpc)
    (setq doc-view-ghostscript-program "mgs.exe")))

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
  (use-package init-elfeed)) 

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
  :ensure t
  :after helm
  :config (helm-descbinds-mode))

(use-package help+
  :ensure t
  :after help)

(use-package help-mode+
  :ensure t
  :after help-mode)

(use-package help-fns+
  :after help-fns
  :config
  ;; Add Org manual to list of manuals to include links for in help.
  ;; This is a stopgap measure.  In the long run, keying in to
  ;; `Info-file-list-for-emacs' is probably the way to go.
  (setq help-cross-reference-manuals '(("emacs" "elisp" "org"))))

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand) ; M-/
  :init
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
  (add-hook 'lisp-tag-hook #'aph/hippie-expand-config-lisp))

(use-package info
  :defer t
  :config
  ;; Improves functionality of `C-h F' for Org commands.
  (add-to-list 'Info-file-list-for-emacs "org")) 

(use-package lisp-mode
  :defer t 
  :config
  ;; Mode tags
  (aph/mode-tag-add 'lisp-mode             'lisp)
  (aph/mode-tag-add 'emacs-lisp-mode       'lisp)
  (aph/mode-tag-add 'lisp-interaction-mode 'lisp))

(use-package minibuffer
  :defer t
  :config
  (setq completion-auto-help 'lazy)) 

(use-package org 
  :ensure t 
  :config
  (use-package init-org))

(use-package org-mobile
  :disabled t
  :config
  (setq org-mobile-directory       "~/sync/mobile"
      org-mobile-inbox-for-pull (concat org-directory "/capture.org"))
  (when (eq aph/machine 'mpc)
    (setq org-mobile-checksum-binary
          "C:/Program Files (Portable)/GnuWin Core Utilities/bin/sha1sum.exe")))

(use-package page
  :defer t
  :config
  (put 'narrow-to-page  'disabled nil))

(use-package paren
  :config
  (show-paren-mode))

(use-package projectile
  :after helm
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system     'helm
        projectile-switch-project-action #'helm-projectile
        projectile-enable-caching        t)
  (helm-projectile-on))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'lisp-tag-hook #'rainbow-delimiters-mode))

(use-package rect
  ;; Move from "C-x r" prefix to "C-c r"
  :bind (("C-x r N" . nil)
         ("C-x r c" . nil)
         ("C-x r d" . nil)
         ("C-x r k" . nil)
         ("C-x r o" . nil)
         ("C-x r r" . nil)
         ("C-x r t" . nil)
         ("C-x r y" . nil)
         ("C-c r N" . rectangle-number-lines)
         ("C-c r c" . clear-rectangle)
         ("C-c r d" . delete-rectangle)
         ("C-c r k" . kill-rectangle)
         ("C-c r o" . open-rectangle)
         ("C-c r r" . copy-rectangle-to-register)
         ("C-c r t" . string-rectangle)
         ("C-c r y" . yank-rectangle)))

(use-package aph-rect
  :after rect
  :bind ("C-c r C-y" . aph/yank-rectangle-from-kill-ring)
  :config
  (when (>= emacs-major-version 25)
    (advice-add #'rectangle--*-char :around #'aph/rectangle-repetition-fix)))

(use-package register
  :bind (("C-x r a" . append-to-register)))

(use-package saveplace
  :config
  (setq-default save-place t) 
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package server
  :config
  (unless (server-running-p) (server-start))
  (setq server-window 'pop-to-buffer))

(use-package shr
  :defer t
  :config
  (use-package aph-shr)
  (advice-add #'shr-urlify :before #'aph/shr-urlify-advice))

(use-package simple
  :bind (("C-S-o"   . join-line)
         ("C-M-/"   . undo-only)
         ("C-S-k"   . kill-whole-line)  ; Was at C-S-<backspace>
         ("C-x M-k" . append-next-kill) ; Was at C-M-w
         ("M-="     . nil)              ; Move to M-= w; see below
         ("M-= w"   . count-words)
         ("M-= l"   . what-line))
  :demand t
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
        eval-expression-print-level  nil)) 

(use-package aph-simple
  :after simple
  :bind ([remap open-line] . aph/open-line) ; C-o
  :config
  (when (eq aph/machine 'mpc)
    (setq aph/eval-expression-clean-output t)))

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

(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode 1))

(use-package solar
  :defer t
  :config
  (setq calendar-longitude -93.2
        calendar-latitude   45.0))

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

(use-package tooltip
  :config
  (tooltip-mode -1)) 

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

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

(use-package aph-window
  :bind (("<up>"   . aph/scroll-down-by-line)
         ("<down>" . aph/scroll-up-by-line)))

(use-package winner
  :config
  (winner-mode)) 

(use-package xahk-mode
  :mode "\\.ahk\\'") 


;;; Not-Yet Refactored
;;;===================
(aph/require-softly 'init-keys)
(aph/require-softly 'init-startup)

;; Remember to sort out aph-geog.el. Need to do this at work.

(message "Initialization complete!")
(message "-----------------------------------")

(provide 'init-core)
(provide 'init)
