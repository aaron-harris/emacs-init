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


;;; Mode Tags
;;;==========
(use-package aph-mode-tag)
(aph/mode-tag-create 'clojure
  "Tag for modes used to edit Clojure, including REPLs.")
(aph/mode-tag-create 'lisp
  "Tag for modes used to edit any sort of Lisp, including REPLs.")


;;; Prefix Maps
;;;============
(defvar aph/buffer-info-map (make-sparse-keymap)
  "Keymap for commands presenting info about current buffer.")
(defalias 'aph/buffer-info-prefix aph/buffer-info-map)

(defvar aph/launch-map (make-sparse-keymap)
  "Keymap for commands launching new \"apps\" within Emacs.")
(defalias 'aph/launch-prefix aph/launch-map)

(defvar aph/region-manip-map (make-sparse-keymap)
  "Keymap for commands manipulating the region.")
(defalias 'aph/region-manip-prefix aph/region-manip-map)
(bind-key "s-<apps>" #'aph/region-manip-prefix)


;;; Configuration: Source-Level
;;;===========================
(prefer-coding-system 'utf-8-unix)

;; UI Configuration
(setq-default cursor-type                     'box
              frame-resize-pixelwise          t
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
(bind-keys
 ("<up>"   . scroll-down-line)
 ("<down>" . scroll-up-line)
 ("s-]"    . other-window)
 ("C-S-t"  . transpose-paragraphs))
(bind-keys :map aph/region-manip-map
           ("k" .   flush-lines)
           ("M-k" . keep-lines))


;;; Package Configuration
;;;======================
(use-package aph-theme
  :bind ("s-n" . aph/theme-cycle))

(use-package avoid
  :init
  (mouse-avoidance-mode 'banish)
  :config
  (setq mouse-avoidance-banish-position
        '((frame-or-window . frame)
          (side . right)
          (side-pos . -50)
          (top-or-bottom . bottom)
          (top-or-bottom-pos . -50))))

(use-package avy
  :ensure t
  :bind (("M-g M-q" . avy-goto-char-2)
         ("M-g q"   . avy-goto-char)
         ("M-g M-g" . avy-goto-line)
         ("M-g M-w" . avy-goto-word-or-subword-1))
  :bind (:map isearch-mode-map
              ("M-g" . avy-isearch))
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

(use-package browse-url
  :bind ("C-c C-o" . browse-url))

(use-package calc
  :bind (:map aph/launch-map
              ("C-c" . calc)))

(use-package cider
  :ensure t
  :defer t
  :config
  (bind-keys (:map cider-mode-map
                   ("C-h A" . cider-apropos)
                   ("C-h D" . cider-apropos-documentation)))
  (setq cider-auto-select-error-buffer      nil
        cider-show-error-buffer             'except-in-repl
        cider-repl-pop-to-buffer-on-connect nil)
  (aph/mode-tag-add 'cider-repl-mode 'lisp)
  (aph/mode-tag-add 'cider-repl-mode 'clojure)
  (add-to-list 'aph/help-window-names "*cider-doc*")
  ;; Output from the JVM has Windows-style newlines, so we need to
  ;; strip those unless we want to see ^M characters in Cider buffers.
  (use-package aph-w32)
  (add-hook 'cider-repl-mode-hook            #'aph/remove-dos-eol)
  (add-hook 'cider-macroexpansion-mode-hook  #'aph/remove-dos-eol)
  (add-hook 'cider-test-report-mode-hook     #'aph/remove-dos-eol))

(use-package clojure-mode
  :ensure t
  :defer t 
  :config
  (aph/mode-tag-add 'clojure-mode 'lisp)
  (aph/mode-tag-add 'clojure-mode 'clojure)
  (add-hook 'clojure-tag-hook #'subword-mode))

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

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'lisp-tag-hook #'eldoc-mode))

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
  ;; Backup file location
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups")))))

(use-package aph-files
  :after files
  :bind (("C-x k"   . aph/kill-active-buffer)
         ("C-x C-c" . aph/delete-frame-or-exit))
  :config
  ;; Make Emacs source read-only
  (when (eq aph/machine 'mpc)
    (setq aph/emacs-source-dirs
          '("C:/Program Files (Portable)/Emacs/share/emacs")))
  (aph/emacs-source-make-read-only))

(use-package find-func
  :bind ("C-x M-l" . find-library))

(use-package font-lock
  :defer t
  :config
  (eval-after-load 'dash        #'dash-enable-font-lock)
  (eval-after-load 'aph-require #'aph/require-enable-font-lock))

(use-package frame
  :bind (("C-z" . aph/launch-prefix))) 

(use-package helm
  :ensure t
  :demand t
  :bind (("M-x"                    . helm-M-x)
         ("C-M-:"                  . helm-eval-expression-with-eldoc)
         ("M-y"                    . helm-show-kill-ring)
         ("C-x r i"                . helm-register)
         ("C-c b b"                . helm-filtered-bookmarks)
         ("C-c ,"                  . helm-semantic-or-imenu)
         ("M-s o"                  . helm-occur)
         ("M-s r"                  . helm-regexp)
         ("C-c SPC"                . helm-all-mark-rings)
         ([remap switch-to-buffer] . helm-mini)
         ([remap find-file]        . helm-find-files)
         ("C-x M-p"                . helm-browse-project)
         ("M-s g"                  . helm-do-grep)
         ("C-h C-f"                . helm-colors)
         ([remap manual-entry]     . helm-man-woman)
         ("C-h C-i"                . helm-info-at-point)
         ([remap apropos-command]  . helm-apropos))
  :bind (:map aph/launch-map
              ("C-s" . helm-google-suggest)
              ("M-c" . helm-calcul-expression))
  :bind (:map helm-map
              ("<tab>"    . helm-execute-persistent-action)
              ("C-j"      . nil)
              ("C-z"      . nil)
              ("s-<apps>" . helm-select-action)))

(use-package helm-config 
  :after helm
  :diminish helm-mode
  :bind (("C-x c" . nil)
         ("C-x x" . helm-command-prefix))
  :bind (:map helm-command-map
              ("C-u" . helm-resume))
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq helm-scroll-amount                    8
        helm-split-window-in-side-p           t
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp        t)
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

(use-package helm-projectile
  :ensure t
  :bind (("C-x p"   . helm-projectile)
         ("M-s M-g" . helm-projectile-grep)))

(use-package help
  :bind (("C-h C-h" . nil)))

(use-package help+
  :ensure t
  :after help)

(use-package help-mode+
  :ensure t
  :after help-mode)

(use-package help-fns+
  :after help-fns
  :bind ("C-h M-b" . describe-buffer)
  :bind (:map aph/buffer-info-map       ; M-=
              ("b" . describe-buffer))
  :config
  ;; Undo binding changes at C-h c.
  (bind-key "C-h c" 'describe-key-briefly)
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

(use-package hl-line
  :bind ("C-c h l" . hl-line-mode))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

(use-package ielm
  :defer t
  :config
  (use-package aph-ielm)
  (bind-keys :map ielm-map
             ("C-c C-t" . aph/eval-expression-toggle-clean-output)
             ("C-c M-w" . aph/ielm-copy-last-output))
  (aph/mode-tag-add 'ielm-mode 'lisp)) 

(use-package info
  :defer t
  :config
  ;; Improves functionality of `C-h F' for Org commands.
  (add-to-list 'Info-file-list-for-emacs "org")) 

(use-package aph-keypad
  :bind ("C-<kp-enter>" . aph/keypad-enter-toggle-newline))

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
  :bind (("C-c a"   . aph/org-agenda)
         ("<f1>"    . aph/org-agenda-display-smart-agenda)
         ("C-c c"   . aph/org-capture-in-popout-frame)
         ("C-c l"   . org-store-link)
         ("C-c w"   . aph/org-goto-last-refile) 
         ("C-c t j" . org-clock-goto)
         ("C-c t o" . org-clock-out)
         ("C-c t x" . org-clock-cancel)
         ("C-c t r" . org-clock-in-last))
  :demand t
  :config
  (unbind-key "C-c [" org-mode-map)
  (unbind-key "C-c ]" org-mode-map)
  (bind-keys :map org-mode-map
             ([remap org-goto] . helm-semantic-or-imenu)
             ("C-c t i"        . org-clock-in)
             ("C-c s SPC"      . aph/org-spin-basic)
             ("C-c s M-SPC"    . aph/org-spin-weighted))
  (use-package init-org))

(use-package org-mobile
  :disabled t
  :config
  (setq org-mobile-directory      "~/sync/mobile"
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

(use-package pp
  :bind ("C-:" . pp-eval-expression))

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
  :demand t
  :bind (("C-S-o"   . join-line)
         ("C-M-/"   . undo-only)
         ("C-S-k"   . kill-whole-line)   ; Was at C-S-<backspace>
         ("C-x M-k" . append-next-kill)  ; Was at C-M-w
         ("M-="     . aph/buffer-info-prefix))
  :bind (:map aph/buffer-info-map
              ("w" . count-words)
              ("l" . what-line))
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
  (advice-add #'eval-expression-print-format
              :around #'aph/eval-expression-mute-print-format)
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
  (smartparens-global-mode)
  (add-hook 'lisp-tag-hook #'smartparens-strict-mode)
  :config
  (bind-keys :map smartparens-mode-map
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
             ("C-<left>"               . sp-forward-barf-sexp)
             ("C-<right>"              . sp-forward-slurp-sexp)
             ("C-S-<left>"             . sp-backward-slurp-sexp)
             ("C-S-<right>"            . sp-backward-barf-sexp)
             ;; Kill and Copy
             ([remap kill-sexp]        . sp-kill-sexp)
             ("C-M-w"                  . sp-copy-sexp)
             ("C-S-<backspace>"        . sp-splice-sexp-killing-around)
             ;; Editing
             ([remap transpose-sexps]  . sp-transpose-sexp)
             ;; Unwrap and Splice
             ("C-<delete>"             . sp-unwrap-sexp)
             ("C-<backspace>"          . sp-backward-unwrap-sexp)
             ("M-D"                    . sp-splice-sexp)
             ;; Indentation
             ("M-)"                    . sp-up-sexp)
             ;; Narrowing
             ("C-x n ("                . sp-narrow-to-sexp))
  (bind-keys :map smartparens-strict-mode-map
             (")" . sp-up-sexp)) 
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
  :bind (:map smartscan-map
              ("M-p"   . nil)
              ("M-n"   . nil)
              ("C-M-r" . smartscan-symbol-go-backward)
              ("C-M-s" . smartscan-symbol-go-forward))
  :init
  (global-smartscan-mode 1))

(use-package solar
  :defer t
  :config
  (setq calendar-longitude -93.2
        calendar-latitude   45.0))

(use-package sort
  :bind (:map aph/region-manip-map
              ("d" . delete-duplicate-lines)
              ("s" . sort-lines)))

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
  (add-to-list 'aph/help-window-names "*TeX Help*")
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
  :bind (("s-["    . aph/other-window-backward)
         ("s-{"    . aph/pull-buffer-backward)
         ("s-}"    . aph/slide-buffer-forward)
         ("s-\\"   . aph/swap-buffer-forward-and-ride)
         ("s-|"    . aph/swap-buffer-forward)
         ("C-c q"  . aph/quit-help-windows)))

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
