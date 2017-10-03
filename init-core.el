;;; init-core.el --- Personal Emacs configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Aaron Harris
;; Author: Aaron Harris <meerwolf@gmail.com>

;;; Code:


;;;; Bootstrapping Variables
;;==========================
;; These variables should have been set by the bootstrapper.
(defvar aph/sync-directory "~/sync"
  "The path to my Dropbox folder on the current machine.")

(defvar aph/machine 'default
  "A symbol denoting the specific PC being used.")


;;;; Disable Customization
;;========================
;; Save customized settings to a file which is never loaded.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;;;; Display Settings
;;===================
;; Placing this close to the beginning of initialization should
;; prevent chrome from being drawn at all, rather than drawing it then
;; removing it.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;;; Package Loading
;;==================
(add-to-list 'load-path
             (expand-file-name (concat aph/sync-directory "/emacs/init")))

(require 'package)

(setq package-user-dir
      (expand-file-name (concat aph/sync-directory "/emacs/elpa")))

(setq package-archives
      (and (not (eq aph/machine 'mpc))
           (not (eq aph/machine 'portable))
           '(("elpa"      . "http://elpa.gnu.org/packages/")
             ("marmalade" . "https://marmalade-repo.org/packages/")
             ("melpa"     . "https://melpa.org/packages/"))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package aph-autoloads)


;;;; Library Packages
;;===================
(use-package advice
  :defer t
  :config (validate-setq ad-redefinition-action 'accept))

(use-package dash
  :ensure t
  :defer t
  :config
  (dash-enable-font-lock))

(use-package s
  :ensure t
  :defer t)

(use-package validate
  :ensure t)


;;;; Metaconfiguration Packages
;;=============================
(use-package chimera)

(use-package fix)

(use-package hydra
  :ensure t
  :defer t
  :config
  (validate-setq lv-use-separator t))

(use-package mode-local
  :config
  (put 'setq-mode-local 'lisp-indent-function 1))

(use-package umbra
  :bind (("C-x C-#" . umbra-mode))
  :bind (:map umbra-mode-map
              ("<return>"   . umbra-default-return-command)
              ("<kp-enter>" . umbra-default-kp-enter-command)
              ("<tab>"      . umbra-default-tab-command))
  :init (umbra-mode))


;;;; Configuration: Source-Level
;;==============================
(prefer-coding-system 'utf-8-unix)
(validate-setq enable-recursive-minibuffers t)

;; Personal Information
(validate-setq user-full-name    "Aaron Harris"
               user-mail-address "meerwolf@gmail.com")

;; UI Configuration
(validate-setq cursor-type                     'box
               frame-resize-pixelwise          t
               indicate-buffer-boundaries      'right
               inhibit-startup-screen          t
               ring-bell-function              #'ignore
               scroll-conservatively           1000
               scroll-margin                   1
               scroll-preserve-screen-position :always)
(setq          resize-mini-windows             t) ;; Won't validate on Emacs 25
(setq-default  indent-tabs-mode                nil)

;; Use Windows keys for super modifier.
(validate-setq w32-lwindow-modifier       'super
               w32-rwindow-modifier       'super
               w32-pass-lwindow-to-system nil
               w32-pass-rwindow-to-system nil)

;; Enable some disabled commands
(dolist (command '(downcase-region
                   narrow-to-region
                   upcase-region))
  (put command 'disabled nil))

;; Keybindings for source-defined commands
(bind-keys :map umbra-mode-map
           ("<up>"           . scroll-down-line)
           ("<down>"         . scroll-up-line)
           ("C-]"            . other-window) 
           ("M-i"            . indent-relative)
           ("C-M-g"          . abort-recursive-edit)
           ("C-M-\\"         . toggle-input-method)
           ("C-S-t"          . transpose-paragraphs)
           ("s-<apps> c c"   . capitalize-region)
           ("s-<apps> c l"   . downcase-region)
           ("s-<apps> c r"   . upcase-region)
           ("s-<apps> k"     . flush-lines)
           ("s-<apps> M-k"   . keep-lines)
           ("s-<apps> <tab>" . indent-region))


;;;; Basic Packages
;;=================
(use-package align
  :bind (:umbra text-mode
                ("C-M-i" . align-regexp))
  :bind (:umbra prog-mode
                ("C-M-i" . align-regexp)))

(use-package aph-align
  :bind (:umbra text-mode
                ("C-i" . aph/align))
  :bind (:umbra prog-mode
                ("C-i" . aph/align)))

(use-package autoinsert
  :init
  (auto-insert-mode)
  :config
  ;; Disable `helm' because it can't handle elisp keyword selection.
  (with-eval-after-load 'helm-mode
    (add-to-list 'helm-completing-read-handlers-alist
                 '(auto-insert . nil))
    (validate-variable 'helm-completing-read-handlers-alist)))

(use-package avoid
  :init
  (mouse-avoidance-mode 'banish)
  :config
  (validate-setq mouse-avoidance-banish-position
                 '((frame-or-window   . frame)
                   (side              . right)
                   (side-pos          . -50)
                   (top-or-bottom     . bottom)
                   (top-or-bottom-pos . -50))))

(use-package avy
  :ensure t
  :bind (:map umbra-mode-map
              ("M-g M-q" . avy-goto-char-2)
              ("M-g q"   . avy-goto-char)
              ("M-g M-g" . avy-goto-line)
              ("M-g M-w" . avy-goto-word-or-subword-1))
  :bind (:umbra isearch-mode
                ("M-g" . avy-isearch))
  :config
  (validate-setq avy-all-windows     nil
                 avy-background      t
                 avy-highlight-first t
                 avy-style           'pre))

(use-package bind-key
  :bind (:map umbra-mode-map
              ("C-h C-b" . describe-personal-keybindings)))

(use-package bookmark
  ;; Move from "C-x r" prefix to "C-c b"
  :bind (:map umbra-mode-map
              ("C-x r b" . undefined)
              ("C-x r l" . undefined)
              ("C-x r m" . undefined)
              ("C-c b b" . bookmark-jump)
              ("C-c b l" . bookmark-bmenu-list)
              ("C-c b m" . bookmark-set)))

(use-package calc
  :bind (:map umbra-mode-map
              ("C-z C-c" . calc)))

(use-package cde
  :if (eq aph/machine 'mpc)
  :bind (:map umbra-mode-map
              ("C-c C-="   . cde-format)))

(use-package color-identifiers-mode
  :ensure t
  :defer t
  :diminish color-identifiers-mode
  :init
  (add-hook 'lisp-family-hook #'color-identifiers-mode)
  :config
  (validate-setq color-identifiers:num-colors      12
                 color-identifiers:color-luminance 0.65)

  ;; In the function `color-identifiers:clojure-declarations-in-sexp',
  ;; there is a call to `evenp', which is not defined; presumably, the
  ;; package maintainers are using `cl' in their local setup.  As a
  ;; stopgap, I'm aliasing just `evenp' globally.
  (defalias #'evenp #'cl-evenp))

(use-package company
  :ensure t
  :bind (:umbra company-mode
                ("<tab>"   . company-indent-or-complete-common)
                ("C-<tab>" . company-complete-common))
  :diminish company-mode
  :init
  (add-hook 'lisp-family-hook #'company-mode)
  :config
  (bind-keys :map company-active-map
             ("<tab>" . company-complete-common-or-cycle))
  (validate-setq company-idle-delay nil))

(use-package crux
  :ensure t
  :bind
  (:map umbra-mode-map
        ;; Local editing
        ("M-O"           . crux-smart-open-line-above)
        ("C-c d"         . crux-duplicate-current-line-or-region)
        ("C-c M-d"       . crux-duplicate-and-comment-current-line-or-region)
        ;; Global editing
        ("s-<apps> w"    . crux-cleanup-buffer-or-region)
        ;; Kill and yank
        ("C-<backspace>" . crux-kill-line-backwards)
        ("C-S-k"         . crux-kill-whole-line)
        ("M-W"           . crux-indent-rigidly-and-copy-to-clipboard)
        ;; Buffer and file management
        ("C-x <delete>"  . crux-delete-file-and-buffer)
        ("C-x M-s"       . crux-rename-file-and-buffer)
        ("C-;"           . crux-switch-to-previous-buffer)
        ;; File and buffer hotkeys
        ("C-x <tab>"     . indent-rigidly)
        ("C-x C-i"       . crux-find-user-init-file)
        ("C-x C-S-i"     . crux-find-shell-init-file))
  :bind (:umbra emacs-lisp-mode
                ("C-c C-w"   . crux-eval-and-replace)
                ("C-c C-S-z" . crux-create-scratch-buffer)))

(use-package deck
  :bind (:map umbra-mode-map
              ("M-["   . deck-surf-swap-buffer-backward)
              ("M-]"   . deck-surf-swap-buffer-forward)
              ("C-\\"  . deck-push-buffer-forward)
              ("M-\\"  . deck-pull-buffer-backward)))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package doc-view
  :bind (:umbra doc-view-mode
                ("C-v" . doc-view-scroll-up-or-next-page)
                ("M-v" . doc-view-scroll-down-or-previous-page))
  :config
  (setq doc-view-resolution 200)
  ;; On mpc, Ghostview has a different name.
  (when (eq aph/machine 'mpc)
    (validate-setq doc-view-ghostscript-program "mgs.exe")))

(use-package enumerate
  :bind (:map umbra-mode-map
              ("s-<apps> n"   . enumerate-lines)
              ("s-<apps> C-n" . enumerate-alpha)))

(use-package expand-region
  :bind (:map umbra-mode-map ("C-;" . er/expand-region))
  :config
  (validate-setq expand-region-contract-fast-key "'"
                 expand-region-reset-fast-key    " "))

(use-package files
  :defer t
  :config
  (validate-setq
   backup-directory-alist  `(("." . ,(concat user-emacs-directory "backups")))
   confirm-kill-emacs      #'y-or-n-p))

(use-package aph-files
  :after files
  :bind (:map umbra-mode-map
              ("C-x k"        . aph/kill-active-buffer)
              ("C-x C-c"      . aph/delete-frame-or-exit)))

(use-package hippie-exp
  :bind (:map umbra-mode-map
              ([remap dabbrev-expand] . hippie-expand))
  :init
  (setq-family-local lisp
    hippie-expand-try-functions-list
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

(use-package hl-line
  :bind (:map umbra-mode-map
              ("C-c h l" . hl-line-mode)))

(use-package ibuffer
  :bind (:map umbra-mode-map
              ([remap list-buffers] . ibuffer)))

(use-package aph-iimage
  :bind (:umbra iimage-mode
                ("C-c i" . aph/iimage-refresh)))

(use-package jerk
  :if (eq aph/machine 'mpc)
  :bind (:map umbra-mode-map
              ("C-x C-y" . jerk-access-inline)))

(use-package kp-motion
  :bind (:map umbra-mode-map
              ("C-<kp-enter>" . kp-motion-mode)))

(use-package liberate-key
  :bind (:map umbra-mode-map
              ;; Liberating C-M-[ from legacy of escape
              ("<escape> <escape> <escape>" . keyboard-escape-quit)
              ("ESC ESC ESC"                . undefined)
              ("M-ESC :"                    . undefined))
  :demand t
  :config
  ;; Key liberation
  (liberate-key-escape)
  (add-hook 'after-make-frame-functions #'liberate-key-escape)
  ;; Map <kp-enter> to <return> rather than to RET (C-m)
  (define-key function-key-map (kbd "<kp-enter>") (kbd "<return>")))

(use-package minibuffer
  :bind (:map umbra-mode-map
              ("C-<tab>" . completion-at-point))
  :config
  (validate-setq completion-auto-help 'lazy))

(use-package page
  :config
  (put 'narrow-to-page 'disabled nil))

(use-package aph-page
  :commands (aph/hydra-page/forward-page
             aph/hydra-page/backward-page)
  :init
  (bind-keys :map umbra-mode-map
             ([remap forward-page]  . aph/hydra-page/forward-page)
             ([remap backward-page] . aph/hydra-page/backward-page)))

(use-package paren
  :config
  (show-paren-mode))

(use-package populate
  :bind (:map umbra-mode-map
              ("s-<apps> p" . populate-downwards-in-region)))

(use-package prog-mode
  :bind (:umbra prog-mode
                ("M-p" . backward-paragraph)
                ("M-n" . forward-paragraph)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'lisp-family-hook #'rainbow-delimiters-mode))

(use-package rect
  ;; Move from "C-x r" prefix to "C-c r"
  :bind (:map umbra-mode-map
              ("C-x r N" . undefined)
              ("C-x r c" . undefined)
              ("C-x r d" . undefined)
              ("C-x r k" . undefined)
              ("C-x r o" . undefined)
              ("C-x r r" . undefined)
              ("C-x r t" . undefined)
              ("C-x r y" . undefined)
              ("C-c r n" . rectangle-number-lines)
              ("C-c r c" . clear-rectangle)
              ("C-c r d" . delete-rectangle)
              ("C-c r k" . kill-rectangle)
              ("C-c r o" . open-rectangle)
              ("C-c r r" . copy-rectangle-to-register)
              ("C-c r t" . string-rectangle)
              ("C-c r y" . yank-rectangle)))

(use-package aph-rect
  :after rect
  :bind (:map umbra-mode-map
              ("C-M-y" . aph/yank-rectangle-from-kill-ring)))

(use-package register
  :bind (:map umbra-mode-map
              ("C-m"   . copy-to-register)
              ("C-M-m" . increment-register)))

(use-package saveplace
  :config
  (setq-default save-place t)
  (validate-setq save-place-file (concat user-emacs-directory "places")))

(use-package simple
  :demand t
  :bind (:map umbra-mode-map
              ("C-M-/"      . undo-only)
              ("C-`"        . next-error)
              ("C-x M-k"    . append-next-kill)
              ("M-= l"      . what-line)
              ("M-= w"      . count-words)
              ("M-SPC"      . mark-word)
              ("M-`"        . previous-error)
              ("M-o"        . join-line)
              ("S-<return>" . delete-blank-lines)
              ("S-SPC"      . cycle-spacing))
  :config
  ;; Global minor modes
  (column-number-mode 1)
  ;; Misc. settings
  (validate-setq save-interprogram-paste-before-kill t
                 shift-select-mode                   nil)
  ;; Tweaking `eval-expression'
  (validate-setq eval-expression-print-length nil
                 eval-expression-print-level  nil))

(use-package aph-simple
  :after simple
  :bind (:map umbra-mode-map
              ("C-a"             . aph/move-beginning-of-line)
              ([remap open-line] . aph/open-line)))

(use-package smart-tab
  :ensure t
  :diminish smart-tab-mode
  :config
  (global-smart-tab-mode 1)
  (validate-setq smart-tab-disabled-major-modes
                 (remove 'org-mode smart-tab-disabled-major-modes))
  (validate-setq smart-tab-using-hippie-expand        t
                 smart-tab-completion-functions-alist nil)
  ;; Use `hippie-expand' in elisp buffers.
  (validate-setq smart-tab-completion-functions-alist
                 (assq-delete-all 'emacs-lisp-mode
                                  smart-tab-completion-functions-alist)))

(use-package smartparens
  :ensure t
  :bind (:umbra smartparens-mode
                ;; Movement
                ([remap backward-sexp]    . sp-backward-sexp)
                ([remap forward-sexp]     . sp-forward-sexp)
                ([remap backward-up-list] . sp-backward-up-sexp)
                ([remap down-list]        . sp-down-sexp)
                ([remap forward-list]     . sp-next-sexp)
                ([remap backward-list]    . sp-previous-sexp)
                ("M-B"                    . sp-backward-symbol)
                ("M-F"                    . sp-forward-symbol)
                ;; Selection
                ([remap mark-sexp]        . sp-select-next-thing-exchange)
                ;; Barf and Slurp
                ("C-M-["                  . sp-forward-barf-sexp)
                ("C-M-]"                  . sp-forward-slurp-sexp)
                ("M-{"                    . sp-backward-slurp-sexp)
                ("M-}"                    . sp-backward-barf-sexp)
                ("C-{"                    . sp-absorb-sexp)
                ("C-}"                    . sp-emit-sexp)
                ("M-A"                    . sp-extract-before-sexp)
                ("M-E"                    . sp-extract-after-sexp)
                ;; Kill and Copy
                ([remap kill-sexp]        . sp-kill-sexp)
                ("C-M-<backspace>"        . sp-backward-kill-sexp)
                ("C-M-w"                  . sp-copy-sexp)
                ("M-K"                    . sp-splice-sexp-killing-around)
                ;; Editing
                ([remap transpose-sexps]  . sp-transpose-sexp)
                ("M-T"                    . sp-convolute-sexp)
                ;; Unwrap and Splice
                ("M-D"                    . sp-unwrap-sexp)
                ("M-S-<delete>"           . sp-unwrap-sexp)
                ("M-S-<backspace>"        . sp-backward-unwrap-sexp)
                ("M-U"                    . sp-splice-sexp)
                ("M-R"                    . sp-rewrap-sexp)
                ("M-S"                    . sp-split-sexp)
                ("M-J"                    . sp-join-sexp)
                ;; Indentation
                ("C-M-q"                  . sp-indent-defun)
                ;; Narrowing
                ("C-x n ("                . sp-narrow-to-sexp)
                ;; Prefix Arguments
                ("C-S-u SPC"              . sp-prefix-save-excursion)
                ("C-S-u '"                . sp-prefix-symbol-object)
                ("C-S-u ("                . sp-prefix-pair-object)
                ("C-S-u ["                . sp-prefix-pair-object)
                ("C-S-u ,"                . sp-prefix-tag-object))
  :bind (:umbra smartparens-strict-mode
                (")" . sp-up-sexp)
                ("]" . sp-up-sexp)
                ("}" . sp-up-sexp))
  :init
  (require 'smartparens-config)
  (smartparens-global-mode)
  (add-hook 'lisp-family-hook #'smartparens-strict-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-strict-mode)
  :config
  ;; String handling
  (add-to-list 'sp-navigate-consider-stringlike-sexp 'org-mode)
  (add-to-list 'sp-navigate-consider-stringlike-sexp 'lisp-mode)
  (add-to-list 'sp-navigate-consider-stringlike-sexp 'clojure-mode)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  ;; Disable '' pair in minibuffer for `eval-expression'
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  ;; Use `` instead of `' in Clojure mode
  (sp-local-pair 'clojure-mode "`" "`"
                 :when '(sp-in-string-p sp-in-comment-p)))

(use-package aph-smartparens
  :bind (:umbra smartparens-mode
                ("M-k" . aph/sp-kill-sentence)))

(use-package smartscan
  :ensure t
  :bind (:umbra smartscan-mode
                ("C-M-r" . smartscan-symbol-go-backward)
                ("C-M-s" . smartscan-symbol-go-forward)
                ("C-M-'" . smartscan-symbol-replace))
  :init
  (add-hook 'text-mode-hook #'smartscan-mode)
  (add-hook 'prog-mode-hook #'smartscan-mode))

(use-package solar
  :defer t
  :config
  (validate-setq calendar-longitude -93.2
                 calendar-latitude   45.0))

(use-package sort
  :bind (:map umbra-mode-map
              ("s-<apps> d" . delete-duplicate-lines)
              ("s-<apps> s" . sort-lines)))

(use-package source-lock
  :config
  (when (eq aph/machine 'mpc)
    (validate-setq source-lock-directories
                   '("C:/Program Files (Portable)/Emacs/share/emacs")))
  (source-lock-mode t))

(use-package text-mode
  :bind (:umbra text-mode
                ("M-p" . backward-paragraph)
                ("M-n" . forward-paragraph)))

(use-package tidy
  :bind (:map umbra-mode-map
              ("C-c q" . tidy-buffers)))

(use-package tooltip
  :config
  (tooltip-mode -1))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package uniquify
  :config
  ;; Snippet from http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
  (validate-setq uniquify-buffer-name-style   'forward
                 uniquify-separator           "/"
                 uniquify-after-kill-buffer-p t
                 uniquify-ignore-buffers-re   "^\\*"))

(use-package vc
  :config
  (add-to-list 'vc-directory-exclusion-list ".stack-work")
  (validate-variable 'vc-directory-exclusion-list))

(use-package aph-vc
  :bind (:map umbra-mode-map
              ("C-x v <delete>" . aph/vc-delete-file))
  :bind (:umbra vc-dir-mode
                ("<delete>" . aph/vc-dir-delete-file)))

(use-package view
  :bind (:map umbra-mode-map
              ([remap scroll-up-command]   . View-scroll-half-page-forward)
              ([remap scroll-down-command] . View-scroll-half-page-backward)))

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
  (validate-setq visible-mark-max   2
                 visible-mark-faces '(aph/visible-mark-top
                                      aph/visible-mark-other)))

(use-package visual-regexp
  :ensure t
  :bind (:map umbra-mode-map
              ("C-'"   . vr/query-replace)
              ("M-'" . vr/replace))
  :config
  (validate-setq vr/auto-show-help nil))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode 1))

(use-package which-func
  :init
  (which-function-mode 1))

(use-package which-func-header
  :after which-func
  :config
  (add-hook 'prog-mode-hook #'which-func-header-mode)
  (add-hook 'org-mode-hook  #'which-func-header-mode))

(use-package aph-which-func
  :after which-func
  :config
  (add-to-list 'which-func-functions #'aph/which-function-org))

(use-package window
  :bind (:map umbra-mode-map
              ("C-S-l" . move-to-window-line-top-bottom)))

(use-package aph-window
  :bind (:map umbra-mode-map
              ("<C-[>" . aph/other-window-backward)
              ("C-M-v" . aph/hydra-scroll-other/body)))

(use-package winner
  :config
  (winner-mode))

(use-package xahk-mode
  :mode "\\.ahk\\'")


;;;; Emacs Lisp
;;=============
(use-package canary
  :bind (:map umbra-mode-map
              ("C-h h" . canary-hooks)))

(use-package clean-eval
  :bind (:umbra inferior-emacs-lisp-mode
                ("C-'" . clean-eval-mode))
  :init
  (when (eq aph/machine 'mpc)
    (clean-eval-mode t)))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'lisp-family-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ert
  :bind (:umbra emacs-lisp-mode
                ("C-c C-t" . ert))
  :init
  (add-to-list 'load-path (expand-file-name (concat aph/sync-directory
                                                    "/emacs/init/test")))
  (validate-variable 'load-path))

(use-package ielm
  :config
  (mode-family-add 'ielm-mode 'lisp))

(use-package aph-ielm
  :bind (:umbra inferior-emacs-lisp-mode
                ("C-c C-w" . aph/ielm-copy-last-output)))

(use-package ielm-repl
  :bind (:umbra emacs-lisp-mode
                ("C-c C-z" . ielm-repl))
  :bind (:umbra inferior-emacs-lisp-mode
                ("C-c C-z" . ielm-repl-switch-back)))

(use-package lisp-mode
  :bind (:umbra emacs-lisp-mode
                ("C-c p C-u" . update-directory-autoloads))
  :config
  ;; Mode tags
  (mode-family-add 'lisp-mode             'lisp)
  (mode-family-add 'emacs-lisp-mode       'lisp)
  (mode-family-add 'lisp-interaction-mode 'lisp)
  ;; Add `use-package' blocks to Imenu
  (add-to-list 'lisp-imenu-generic-expression
               '("Package"
                 "^(use-package\\s-+\\(\\_<.+?\\_>\\)"
                 1))
  ;; Add `defhydra's to Imenu
  (add-to-list 'lisp-imenu-generic-expression
               '("Hydra"
                 "^(defhydra\\s-+\\(\\_<.+?\\_>\\)"
                 1))
  ;; Add `ert' tests to Imenu
  (add-to-list 'lisp-imenu-generic-expression
               '("Test"
                 "^(ert-deftest\\s-+\\(\\_<.+?\\_>\\)"
                 1))
  (validate-variable 'lisp-imenu-generic-expression))

(use-package aph-lisp-mode
  :after lisp-mode
  :bind (:umbra emacs-lisp-mode
                ("C-c C-l" . aph/eval-region-or-buffer))
  :config
  (validate-setq lisp-indent-function #'aph/lisp-indent-function)
  (add-hook 'emacs-lisp-mode-hook #'aph/emacs-lisp-add-font-lock-keywords))

(use-package pp
  :bind (:umbra emacs-lisp-mode
                ("C-c C-m" . pp-macroexpand-last-sexp))
  :bind (:map umbra-mode-map
              ("C-:"   . pp-eval-expression)
              ("C-M-:" . pp-macroexpand-expression)))


;;;; Helm and Projectile
;;======================
;; Helm core modules
(use-package helm
  :ensure t
  :demand t
  :bind (:map umbra-mode-map
              ("M-x"                    . helm-M-x)
              ("M-y"                    . helm-show-kill-ring)
              ("M-m"                    . helm-register)
              ("C-c b b"                . helm-filtered-bookmarks)
              ("M-."                    . helm-semantic-or-imenu)
              ("M-s o"                  . helm-occur)
              ("M-,"                    . helm-all-mark-rings)
              ([remap switch-to-buffer] . helm-mini)
              ([remap find-file]        . helm-find-files)
              ("M-s g"                  . helm-do-grep)
              ("C-h C-f"                . helm-colors)
              ([remap manual-entry]     . helm-man-woman)
              ("C-h C-i"                . helm-info-at-point)
              ([remap apropos-command]  . helm-apropos)
              ("C-z C-s"                . helm-google-suggest)
              ("C-z M-c"                . helm-calcul-expression)
              ("C-z C-p"                . helm-list-elisp-packages)
              ("C-x ,"                  . helm-resume))
  :bind (:map umbra-mode-helm-map
              ("<tab>"    . helm-execute-persistent-action)
              ("C-j"      . undefined)
              ("C-z"      . undefined)
              ("s-<apps>" . helm-select-action)))

(use-package helm-buffers
  :config
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-recentf
          helm-source-files-in-current-dir
          helm-source-bookmarks
          helm-source-buffer-not-found)))

(use-package helm-config
  :after helm
  :diminish helm-mode
  :bind (:map umbra-mode-map
              ("C-x c"     . undefined)
              ("C-x x"     . helm-command-prefix))
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (validate-setq helm-scroll-amount                    8
                 helm-split-window-in-side-p           t
                 helm-ff-file-name-history-use-recentf t
                 helm-ff-search-library-in-sexp        t)
  ;; Info pages to use for `helm-info-at-point'.
  (put 'helm-info-default-sources 'custom-type '(repeat symbol))
  (validate-setq helm-info-default-sources
                 '(helm-source-info-emacs
                   helm-source-info-elisp
                   helm-source-info-cl
                   helm-source-info-eieio
                   helm-source-info-org
                   helm-source-info-pages))
  ;; Turn Helm off for Org-mode refiling, since Helm can't handle
  ;; multiple levels of refile targets.  We also need to turn off
  ;; capture to use `aph/org-capture-choose-targets'.
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(org-refile  . nil))
  (validate-variable 'helm-completing-read-handlers-alist))

(use-package vizier-helm
  :bind (:map umbra-mode-helm-map
              ("<return>" . vizier-helm-resume-update-or-exit-minibuffer)))

;; Projectile
(use-package projectile
  :after helm
  :ensure t
  :bind (:map umbra-mode-map
              ("C-x M-p" . projectile-switch-project))
  :config
  (projectile-global-mode)
  (validate-setq projectile-completion-system     'helm
                 projectile-switch-project-action #'helm-projectile
                 projectile-enable-caching        t)
  (helm-projectile-on))

(use-package aph-projectile
  :after projectile
  :config
  (bind-keys
   :penumbra prog-mode
   ("C-c C-t" .
    (chimera "chimera/projectile-test-project"
      (when (aph/projectile-call-with-project #'projectile-test-command)
        #'projectile-test-project)))
   ("C-c f t" .
    (chimera "chimera/projectile-find-implementation-or-test-other-window"
      (when (projectile-project-p)
        #'projectile-find-implementation-or-test-other-window))))
  (validate-setq projectile-test-prefix-function #'aph/projectile-test-prefix
                 projectile-test-suffix-function #'aph/projectile-test-suffix)
  ;; Register a project type for this project
  (projectile-register-project-type
   'emacs-init '("init-core.el") nil (lambda () (ert t)))
  (push '(emacs-init . (nil . "-test")) aph/projectile-test-alist))

(use-package helm-projectile
  :ensure t
  :defer t)

(use-package aph-helm
  :bind (:map umbra-mode-map
              ("M-."     . aph/helm-semantic-or-imenu)
              ("C-x p"   . aph/helm-projectile)
              ("M-s M-g" . aph/helm-projectile-grep)))

;; Helm add-ons
(use-package helm-descbinds
  :ensure t
  :after helm
  :config (helm-descbinds-mode))


;;;; Help and Info
;;================
(use-package find-func
  :bind (:map umbra-mode-map
              ("C-h C-M-f" . find-function)
              ("C-h C-M-k" . find-function-on-key)
              ("C-h C-M-v" . find-variable)
              ("C-h M-F"   . find-face-definition)
              ("C-h C-M-l" . find-library)))

(use-package aph-help
  :after help
  :bind (:map umbra-mode-map
              ("C-h C-h" . aph/help-describe-bindings))
  :config
  (add-hook 'help-mode-hook #'activate-mode-local-bindings)
  (setq-mode-local help-mode
    revert-buffer-function
    #'aph/help-mode-revert-buffer))

(use-package help+
  :ensure t
  :after help)

(use-package help-mode+
  :ensure t
  :after help-mode)

(use-package help-fns+
  :after help-fns
  :bind (:map umbra-mode-map
              ("C-h c"   . describe-key-briefly)
              ("C-h M-b" . describe-buffer)
              ("M-= b" . describe-buffer))
  :config
  ;; Add Org manual to list of manuals to include links for in help.
  ;; This is a stopgap measure.  In the long run, keying in to
  ;; `Info-file-list-for-emacs' is probably the way to go.
  (validate-setq help-cross-reference-manuals '(("emacs" "elisp" "org"))))

(use-package info
  :defer t
  :config
  ;; Improves functionality of `C-h F' for Org commands.
  (add-to-list 'Info-file-list-for-emacs "org")
  (validate-variable 'Info-file-list-for-emacs))

(use-package aph-info
  :bind (:map umbra-mode-map
              ("C-h i" . aph/info-mode-or-clone-buffer))
  :bind (:umbra Info-mode
                ("0" . aph/Info-final-menu-item)))


;;;; Org and Outline
;;==================
;; Core modules
(use-package outline
  :bind (:penumbra outline-mode
                   ("C-M-b" . outline-backward-same-level)
                   ("C-M-f" . outline-forward-same-level)
                   ("C-M-n" . outline-next-visible-heading)
                   ("C-M-p" . outline-previous-visible-heading)
                   ("C-M-u" . outline-up-heading)))

(use-package aph-outline
  :bind (:penumbra outline-mode
                   ("C-M-d" . aph/outline-down-heading-from-end)
                   ("C-M-a" . aph/outline-get-first-sibling)
                   ("C-M-e" . aph/outline-get-final-sibling)))

(use-package org
  :ensure t
  :bind (:map umbra-mode-map
              ("C-c l" . org-store-link))
  :bind (:umbra org-mode
                ("C-c [" . undefined)
                ("C-c ]" . org-cycle-agenda-files)
                ("M-."   . helm-org-in-buffer-headings)
                ;; Following bindings restore `org-mode' keys
                ;; unintentionally shadowed by `umbra-mode'
                ("C-o"   . org-open-line))
  :bind (:penumbra org-mode
                   ("C-M-[" . org-metaleft)
                   ("C-M-]" . org-metaright)
                   ("C-M-t" . org-metaup))
  :init
  (custom-set-variables
   ;; Remove strike-through and use ` rather than ~ for code.
   '(org-emphasis-alist '(("*" bold)
                          ("/" italic)
                          ("_" underline)
                          ("=" org-verbatim verbatim)
                          ("`" org-code verbatim))))
  :config
  (message "Loading org...")         ; Because this may take a while.
  (use-package init-org)
  (bind-keys :umbra org-mode
             ("S-<return>" . (chimera "chimera/org-table-copy-down"
                               (when (org-table-p) #'org-table-copy-down))))
  (message "Loading org...done"))

(use-package aph-org
  :after org
  :bind (:map umbra-mode-map
              ("C-="   . aph/org-increase-number)
              ("C-_"   . aph/org-decrease-number)
              ("C-+"   . aph/org-increase-number)
              ("C-c w" . aph/org-goto-last-refile))
  :bind (:umbra org-mode
                ("<tab>" . aph/org-cycle-with-smart-tab)
                ("C-k"   . aph/org-kill-line))
  :bind (:umbra org-agenda-mode
                ("g" . aph/org-agenda-redo))
  :config
  (bind-keys
   :umbra org-mode
   ("C-k" .
    (chimera "chimera/org-clear-or-kill-line"
      (when (org-table-p) #'aph/org-table-clear-row-forward)))
   ("<return>" .
    (chimera "chimera/org-return"
      (when (aph/org-column-view-p) #'next-line))))
  (fixed-scale-mode t)
  (add-to-list 'fixed-scale-command-list #'aph/org-agenda-redo))

;; Agenda
(use-package org-agenda
  :bind (:map umbra-mode-map
              ("C-c a" . org-agenda))
  :bind (:umbra org-agenda-mode
                ("C-o" . org-agenda-open-link)
                ("M-p" . org-agenda-backward-block)
                ("M-n" . org-agenda-forward-block))
  :config
  (validate-setq
   org-agenda-block-separator   (make-string 80 ?=)
   org-agenda-remove-tags       t
   org-agenda-span              'day
   org-agenda-sticky            t
   org-agenda-timegrid-use-ampm t
   org-agenda-window-setup      'current-window
   org-extend-today-until       4)

  (validate-setq
   org-agenda-skip-deadline-if-done                 t
   org-agenda-skip-deadline-prewarning-if-scheduled t
   org-agenda-skip-scheduled-if-done                t
   org-agenda-skip-timestamp-if-done                t
   org-agenda-tags-todo-honor-ignore-options        t
   org-agenda-todo-ignore-timestamp                 nil
   org-agenda-todo-ignore-with-date                 nil)

  (validate-setq
   org-agenda-prefix-format
   '((agenda   . " %i %-13:c%?-12t% s")
     (timeline . "  % s")
     (todo     . " %i %-13:c")
     (tags     . " %i %-13:c")
     (search   . " %i %-13:c")))

  (require 'init-org-agenda))

(use-package aph-org-agenda
  :bind (:umbra org-agenda-mode
                ("<S-right>" . aph/org-agenda-date-later)
                ("<S-left>"  . aph/org-agenda-date-earlier)))

(use-package org-smart-agenda
  :bind (:map umbra-mode-map
              ("<f1>" . org-smart-agenda))
  :config
  (validate-setq org-smart-agenda-views   '("dm" "dw" "de" "ds")
                 org-smart-agenda-workday '(10 . 18.5)))

(use-package org-agenda-sticky
  :after org-agenda)

(use-package org-habit
  :after org-agenda
  :config
  (validate-setq org-habit-graph-column 50))

;; Ancillary modules
(use-package ob-core
  :defer t
  :config
  (validate-setq org-confirm-babel-evaluate nil))

(use-package org-capture
  :defer t
  :config
  (require 'init-org-capture))

(use-package org-clock
  :bind (:map umbra-mode-map
              ("C-c t j" . org-clock-goto)
              ("C-c t o" . org-clock-out)
              ("C-c t x" . org-clock-cancel)
              ("C-c t r" . org-clock-in-last))
  :bind (:umbra org-mode
                ("C-c t i" . org-clock-in)))

(use-package org-display
  :after org
  :bind (:map umbra-mode-map
              ("C-c c" . org-display-capture-in-popout-frame))
  :config
  (require 'bfw)
  (validate-setq
   org-display-todo-placement-action '(bfw-display-buffer-in-subwindow)))

(use-package org-eww
  :after org)

(use-package org-multitheme
  :after org)

(use-package org-spin
  :bind (:umbra org-mode
                ("C-c s"     . org-spin-weighted)))


;;;; Forms Mode
;;=============
(use-package forms
  :bind (:map umbra-mode-map
              ("C-c f f" . forms-find-file))
  :bind (:umbra forms-mode
                ("C-M-p"      . forms-prev-record)
                ("C-M-n"      . forms-next-record)
                ("C-M-a"      . forms-first-record)
                ("C-M-e"      . forms-last-record)
                ("C-c C-a"    . forms-first-record)
                ("C-c C-e"    . forms-last-record)
                ("C-<return>" . forms-insert-record))
  :config
  (validate-setq forms-insert-after t))

(use-package aph-forms
  :after forms
  :config
  (add-hook 'forms-mode-hook #'aph/forms-show-minor-modes))

(use-package forms-random
  :bind (:umbra forms-mode
                ("C-M-." . forms-random-record-weighted)))

(use-package forms-narrow
  :init
  (add-hook 'forms-mode-hook #'forms-narrow-shadow))

(use-package helm-forms
  :bind (:umbra forms-mode
                ("M-." . helm-forms-records)))


;;;; Web Browsing
;;===============
(use-package browse-url
  :bind (:map umbra-mode-map
              ("C-z <return>" . browse-url)))

(use-package browse-url-prefix
  :after browse-url
  :config
  (validate-setq
   browse-url-browser-function                #'browse-url-prefix
   browse-url-prefix-default-browser-function #'eww-browse-url))

(use-package eww
  :bind (:map umbra-mode-map ("C-z C-w" . eww))
  :bind (:umbra eww-mode
                ("x"       . aph/kill-active-buffer)
                ("S-<tab>" . shr-previous-link)
                ("["       . eww-previous-url)
                ("]"       . eww-next-url)
                ("z"       . shr-zoom-image)
                ("M-p"     . backward-paragraph)
                ("M-n"     . forward-paragraph)))

(use-package shr
  :defer t
  :config
  ;; Disable use of proportional fonts in `eww' and other `shr' modes
  (validate-setq shr-use-fonts nil))

(use-package shr-link-img
  :after shr)

;; Elfeed
(use-package elfeed
  :ensure t
  :bind (:map umbra-mode-map
              ("C-z C-f" . elfeed))
  :bind (:umbra elfeed-search-mode
                ("U" . elfeed-unjam))
  :bind (:umbra elfeed-show-mode
                ("M-p" . backward-paragraph)
                ("M-n" . forward-paragraph))
  :config
  ;; Mode family setup (so we can address both Elfeed modes together)
  (mode-family-create 'elfeed)
  (mode-family-add 'elfeed-search-mode 'elfeed)
  (mode-family-add 'elfeed-show-mode   'elfeed)

  ;; UI config
  (validate-setq elfeed-sort-order    'ascending
                 elfeed-search-filter "@6-months-ago +unread -tier2 -tier3")

  ;; Browser config
  (setq-family-local elfeed
    browse-url-prefix-browser-function         #'eww-browse-url
    browse-url-prefix-default-browser-function #'browse-url-default-browser)

  ;; Network config
  (validate-setq url-queue-parallel-processes 1
                 url-queue-timeout            30)

  ;; Filesystem config
  (validate-setq elfeed-db-directory (concat aph/sync-directory "/elfeed"))

  ;; Load the feed list
  (load (expand-file-name (concat elfeed-db-directory "/feeds.el")))
  (validate-variable 'elfeed-feeds))

(use-package elfeed-barb
  :defer t)

(use-package elfeed-lens
  :bind (:umbra elfeed-search-mode
                ("'" . elfeed-lens-cycle))
  :config
  (validate-setq elfeed-lens-list
                 '("@6-months-ago +unread -tier2 -tier3"
                   "@6-months-ago +unread +tier2"
                   "@6-months-ago +unread +tier3"
                   "+todo")))

(use-package elfeed-link
  :after elfeed
  :bind (:umbra eww-mode
                ("p" . elfeed-show-prev)
                ("n" . elfeed-show-next))
  :config
  (validate-setq elfeed-link-tag              'link
                 elfeed-link-browser-function #'eww-browse-url))

(use-package elfeed-sync
  :bind (:umbra elfeed-search-mode
                ("C-x C-s" . elfeed-sync-save)))


;;;; AUCTeX
;;=========
(use-package latex
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (validate-setq
   LaTeX-math-list
   '(("'" (lambda () (interactive) (insert "^{\\prime}")) "Math" nil))))

(use-package preview
  :after tex
  :config
  (validate-setq preview-image-type          'dvipng
                 preview-preserve-counters   t
                 preview-auto-cache-preamble t))

(use-package reftex
  :defer t
  :init
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

(use-package tex
  :ensure auctex
  :defer t
  :config
  ;; Path setup
  (let ((texpath "/usr/local/texlive/2015/bin/x86_64-cygwin"))
    (setenv "PATH" (concat texpath ":" (getenv "PATH")))
    (add-to-list 'exec-path texpath))
  ;; Master file settings
  (setq-default TeX-master nil)
  ;; Caching settings
  (validate-setq TeX-auto-save  t
                 TeX-parse-self t)
  ;; Outline settings
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  ;; Use Emacs as viewer
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Emacs"))
  (add-to-list 'TeX-view-program-list
               '("Emacs" "emacsclient -n %o"))
  ;; Miscellaneous settings
  (with-eval-after-load 'tidy
    (add-to-list 'tidy-unwanted-buffer-list "*TeX Help*"))
  (setq-mode-local TeX-mode
    fill-column 75))


;;;; Clojure
;;==========
(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (mode-family-add 'clojure-mode 'lisp)
  (mode-family-add 'clojure-mode 'clojure)
  (add-hook 'clojure-family-hook #'subword-mode))

(use-package cider
  :ensure t
  :bind (:umbra cider-mode
                ("C-c C-l" . cider-load-buffer)
                ("C-h A"   . cider-apropos)
                ("C-h D"   . cider-apropos-documentation))
  :config
  (validate-setq cider-auto-select-error-buffer      nil
                 cider-repl-pop-to-buffer-on-connect nil
                 cider-show-error-buffer             'except-in-repl)
  (mode-family-add 'cider-repl-mode 'lisp)
  (mode-family-add 'cider-repl-mode 'clojure)
  (with-eval-after-load 'tidy
    (add-to-list 'tidy-unwanted-buffer-list "*cider-doc*"))
  ;; Output from the JVM has Windows-style newlines, so we need to
  ;; strip those unless we want to see ^M characters in Cider buffers.
  (require 'cygwinize)
  (add-hook 'cider-repl-mode-hook            #'cygwinize-hide-dos-eol)
  (add-hook 'cider-macroexpansion-mode-hook  #'cygwinize-hide-dos-eol)
  (add-hook 'cider-test-report-mode-hook     #'cygwinize-hide-dos-eol))


;;;; Haskell
;;==========
(use-package haskell-mode
  :ensure t
  :bind (:umbra haskell-mode
                ("C-c C-l" . haskell-process-load-file)
                ("C-c C-z" . haskell-interactive-switch))
  :config
  ;; REPL setup
  ;;
  ;; This next option is necessary in projects with multiple build
  ;; targets, because GHCI wants to know which target to use (even
  ;; though we don't care), and something about the timing gets
  ;; screwed up.
  (add-to-list 'haskell-process-args-stack-ghci "--no-load")
  (validate-setq haskell-process-show-debug-tips nil
                 haskell-process-log             t)
  ;; Auxiliary features
  (add-hook 'haskell-mode-hook #'eldoc-mode)
  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
  ;; Cygwinization
  (when (eq system-type 'cygwin)
    (require 'cygwinize)
    (cygwinize #'haskell-process-load-file)))

(use-package aph-haskell
  :after haskell-mode
  :config
  ;; Projectile configuration
  (with-eval-after-load 'projectile
    (projectile-register-project-type
     'haskell-stack
     '("stack.yaml")
     #'haskell-compile
     #'aph/haskell-test)))

(use-package haskell-compile
  :bind (:umbra haskell-mode
                ("C-c C-c" . haskell-compile))
  :config
  (validate-setq haskell-compile-cabal-build-command "stack build")
  (add-to-list 'process-coding-system-alist '("stack" . utf-8-dos)))

(use-package haskell-doc
  :after haskell-mode
  :config
  ;; Disable use of unicode symbols in Haskell eldoc until I sort out
  ;; my font issues.
  (validate-setq haskell-doc-prettify-types nil))

(use-package haskell-interactive-mode
  :bind (:umbra haskell-interactive-mode
                ("C-c M-o" . haskell-interactive-mode-clear)))


;;;; Idris
;;========
(use-package idris-mode
  :ensure t
  :bind (:umbra idris-mode
                ("C-c C-c" . idris-ipkg-build))
  :config
  ;; Cygwinization
  (when (eq system-type 'cygwin)
    (require 'cygwinize)
    (cygwinize #'idris-filename-to-load)))


;;;; Themes
;;=========
(use-package aph-theme
  :defer t
  :init
  (add-to-list
   'custom-safe-themes
   "b50769e9fcc86519e961fe7614b5577368701c392afbb8604d5f1b07e2b5b7ac")
  (validate-variable 'custom-safe-themes))

(use-package hc-zenburn-theme
  :defer t
  :ensure t
  :init
  (add-to-list
   'custom-safe-themes
   "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f")
  :config
  (hc-zenburn-with-color-variables
    (custom-theme-set-faces
     'hc-zenburn
     ;; The `hc-zenburn' theme lacks definitions for the the `avy'
     ;; faces.  These specs are translated directly from those in the
     ;; `zenburn' theme.
     `(avy-background-face
       ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg
                        :inverse-video nil))))
     `(avy-lead-face-0
       ((t (:foreground ,hc-zenburn-green+3 :background ,hc-zenburn-bg
                        :inverse-video nil))))
     `(avy-lead-face
       ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg
                        :inverse-video nil))))
     ;; The `volatile-highlights' face should be easier to see.
     `(vhl/default-face
       ((t (:background ,hc-zenburn-bg-2)))))))

(use-package multitheme
  :bind (:map umbra-mode-map
              ("s-n" . multitheme-cycle))
  :init
  (add-hook 'after-init-hook #'multitheme-cycle)
  :config
  (validate-setq multitheme-base-theme-list '(hc-zenburn zenburn)
                 multitheme-overtheme       'aph))

(use-package zenburn-theme
  :defer t
  :ensure t
  :init
  (add-to-list
   'custom-safe-themes
   "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d")
  :config
  (zenburn-with-color-variables
   (custom-theme-set-faces
    'zenburn
    ;; The `volatile-highlights' face should be easier to see.
    `(vhl/default-face
      ((t (:background ,zenburn-bg-1)))))))


;;;; Bug fixes
;;============
(defun aph/lisp-completion-at-point-fix (orig-fn &rest args)
  "Advice so completion of Lisp symbols works inside `'.

The function `lisp-completion-at-point' has a bug that prevents
completion from working properly when attempting to complete a
symbol inside `' quotes.  This advice fixes that bug.

Intended as :around advice for `lisp-completion-at-point'."
  ;; The bug is just that the local `end' variable is calculated
  ;; incorrectly, including the terminating ' character.  Our rather
  ;; heavy-handed solution affects all calls to `forward-sexp', but
  ;; seems to work okay.
  (require 'vizier)
  (vizier-with-advice
      ((forward-sexp
        :after (lambda (&rest _args)
                 (when (eq (char-before (point)) ?')
                   (backward-char 1)))))
    (apply orig-fn args)))

(advice-add 'lisp-completion-at-point :around
            #'aph/lisp-completion-at-point-fix)

(defun aph/org-agenda-quit-fix (orig-fn)
  "Advice so `org-agenda-quit' buries sticky agendas properly.

When `org-agenda-sticky' is true and `org-agenda-window-setup' is
the symbol `reorganize-frame', `org-agenda-quit' has a bug where
the agenda does not get buried properly.  This advice restores
the intended behavior.

Intended as :around advice for `org-agenda-quit'."
  ;; The bug seems to stem from the fact that `bury-buffer' only
  ;; removes the current buffer from its window if its argument is
  ;; nil, not if its argument is the current buffer.  So we
  ;; temporarily advice `bury-buffer' so that it only gets an argument
  ;; when that argument is not current.
  (require 'vizier)
  (vizier-with-advice-if org-agenda-sticky
      ((bury-buffer :filter-args
                    (lambda (args)
                      (unless (equal args (list (current-buffer)))
                        (list (current-buffer))))))
    (funcall orig-fn)))

(advice-add 'org-agenda-quit :around #'aph/org-agenda-quit-fix)

(defun aph/org-agenda-skip-if-fix (oldfun subtree conditions)
  "Advice patching the \"bad negation\" bug in `org-agenda-skip-if'.

The function `org-agenda-skip-if' does not behave correctly when
given multiple conditions, at least one of which is a
negative (e.g., `notscheduled' or `nottodo').  This function
corrects the problem when applied as :around advice."
  (require 'noflet)
  (let ((use-excursion t))
    (noflet ((re-search-forward (&rest args)
				(if use-excursion
				    (save-excursion (apply this-fn args))
				  (apply this-fn args)))
	     (outline-next-heading (&rest args)
				   (let ((use-excursion nil)) (apply this-fn args))))
      (funcall oldfun subtree conditions))))

(advice-add 'org-agenda-skip-if :around #'aph/org-agenda-skip-if-fix)

(defun aph/projectile-find-matching-fix (orig-fn file)
  "Bug-fixing advice for `projectile-find-matching-*'.

The functions `projectile-find-matching-test' and
`projectile-find-matching-file' do not respect the variables
`projectile-test-prefix-function' and
`projectile-test-suffix-function'.  This advice corrects this
deficit, and is intended as :around advice."
  (require 'vizier)
  (vizier-with-advice
      ((:once projectile-test-prefix :override
              (lambda (project-type)
                (funcall projectile-test-prefix-function project-type)))
       (:once projectile-test-suffix :override
              (lambda (project-type)
                (funcall projectile-test-suffix-function project-type))))
    (funcall orig-fn file)))

(dolist (sym '(projectile-find-matching-test
               projectile-find-matching-file))
  (advice-add sym :around #'aph/projectile-find-matching-fix))

(defun aph/rectangle-repetition-fix (fn cmd n &optional other-cmd)
  "Advice to fix bug in `rectangle-mark-mode' motion commands.

The basic cursor motion commands in `rectangle-mark-mode' that
were introduced in Emacs 25 do not currently handle their prefix
arguments correctly (as of Emacs 25.0.50.1).  These
commands (principally `rectangle-forward-char' and
`rectangle-backward-char') delegate to `rectangle--*-char'.  This
function fixes the problem when used as advice :around
`rectangle--*-char'."
  (cond
   ((< n 0)  (aph/rectangle-repetition-fix fn other-cmd (- n)))
   ((= n 0)  (funcall fn cmd 0 other-cmd))
   ((> n 0)  (dotimes (i (1- n) (funcall fn cmd 1 other-cmd))
               (funcall fn cmd 1 other-cmd)))))

(when (>= emacs-major-version 25)
  (advice-add #'rectangle--*-char :around #'aph/rectangle-repetition-fix))

(provide 'init-core)
(provide 'init)
;;; init-core.el ends here
