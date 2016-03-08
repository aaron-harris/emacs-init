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

(require 'package)

(defun aph/offline-p ()
  "Return non-nil if this machine should not install packages.
The return value depends only on `aph/machine'."
  (eq aph/machine 'mpc))

(setq package-user-dir "~/sync/emacs/elpa") 
(setq package-archives
      (and (not (aph/offline-p))
           '(("elpa"      . "http://elpa.gnu.org/packages/")
             ("marmalade" . "https://marmalade-repo.org/packages/")
             ("melpa"     . "https://melpa.org/packages/"))))
(package-initialize)

;; Bootstrap `use-package'
(unless (or (package-installed-p 'use-package) (aph/offline-p))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package aph-autoloads)


;;; Metaconfiguration Packages
;;;===========================
;; Packages in this section are loaded early so we can use them in
;; subsequent package declarations.

(use-package aph-keys
  :bind (("C-x C-#" . aph-keys-mode))
  :bind (:map aph-keys-mode-map
              ("<return>"                   . aph-keys-default-return-command)
              ("<tab>"                      . aph-keys-default-tab-command)
              ;; Liberating C-M-[ from legacy of escape
              ("<escape> <escape> <escape>" . keyboard-escape-quit)
              ("ESC ESC ESC"                . undefined)
              ("M-ESC :"                    . undefined))
  :demand t
  :config
  ;; Key liberation
  (aph/keys-liberate-escape)
  (add-hook 'after-make-frame-functions #'aph/keys-liberate-escape)
  ;; Map <kp-enter> to <return> rather than to RET (C-m) 
  (define-key function-key-map (kbd "<kp-enter>") (kbd "<return>"))
  ;; Personal keybinding mode
  (aph-keys-mode))

(use-package hydra
  :ensure t
  :defer t
  :config
  (setq lv-use-separator t))


;;; Mode Tags
;;;==========
(use-package aph-mode-tag)
(aph/mode-tag-create 'clojure
  "Tag for modes used to edit Clojure, including REPLs.")
(aph/mode-tag-create 'lisp
  "Tag for modes used to edit any sort of Lisp, including REPLs.")


;;; Configuration: Source-Level
;;;============================
(prefer-coding-system 'utf-8-unix)

;; UI Configuration
(setq-default cursor-type                     'box
              frame-resize-pixelwise          t
              indent-tabs-mode                nil
              indicate-buffer-boundaries      'right
              inhibit-startup-screen          t
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
(bind-keys :map aph-keys-mode-map
           ("<up>"           . scroll-down-line)
           ("<down>"         . scroll-up-line)
           ("C-]"            . other-window)
           ("C-M-g"          . abort-recursive-edit)
           ("C-S-t"          . transpose-paragraphs)
           ("C-M-\\"         . toggle-input-method)
           ("s-<apps> k"     . flush-lines)
           ("s-<apps> M-k"   . keep-lines) 
           ("C-'"            . query-replace)
           ("M-'"            . query-replace-regexp)
           ("s-<apps> <tab>" . indent-region)
           ("M-i"            . indent-relative))


;;; Package Configuration
;;;======================
(use-package align
  :bind (:augment (text-mode prog-mode)
                  ("C-M-i" . align-regexp)))

(use-package aph-align
  :bind (:augment (text-mode prog-mode)
                  ("C-i" . aph/align)))

(use-package aph-mpc
  :if (eq aph/machine 'mpc)
  :bind (:map aph-keys-mode-map
              ("C-x C-y"   . aph/mpc-yank-access-inline)
              ("C-x C-S-y" . aph/mpc-yank-access-overfull)
              ("C-z C-="   . aph/mpc-calc-bar)))

(use-package aph-number-lines
  :bind (:map aph-keys-mode-map
         ("s-<apps> n"       . aph/number-lines)
         ("s-<apps> C-n"     . aph/number-lines-alpha)
         ("s-<apps> M-n o"   . aph/number-lines-open)
         ("s-<apps> M-n M-o" . aph/number-lines-open-multiple)))

(use-package aph-theme
  :bind (:map aph-keys-mode-map
              ("s-n" . aph/theme-cycle))
  :demand t
  :config
  ;; Treat 'aph theme as safe:
  (add-to-list
   'custom-safe-themes
   "416ef2f2057400db7cab91aeacb583b4b539c549f4713260282a903d79344312") 
  (load-theme 'aph :noconfirm))

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
  :bind (:map aph-keys-mode-map
              ("M-g M-q" . avy-goto-char-2)
              ("M-g q"   . avy-goto-char)
              ("M-g M-g" . avy-goto-line)
              ("M-g M-w" . avy-goto-word-or-subword-1))
  :bind (:augment isearch-mode
                  ("M-g" . avy-isearch))
  :config
  (setq avy-all-windows     nil
        avy-background      t
        avy-highlight-first t
        avy-style           'pre))

(use-package bookmark
  ;; Move from "C-x r" prefix to "C-c b"
  :bind (:map aph-keys-mode-map
              ("C-x r b" . undefined)
              ("C-x r l" . undefined)
              ("C-x r m" . undefined)
              ("C-c b b" . bookmark-jump)
              ("C-c b l" . bookmark-bmenu-list)
              ("C-c b m" . bookmark-set)))

(use-package browse-url
  :bind (:map aph-keys-mode-map
              ("C-z <return>" . browse-url)))

(use-package calc
  :bind (:map aph-keys-mode-map
              ("C-z C-c" . calc)))

(use-package cider
  :ensure t
  :bind (:augment cider-mode
                  ("C-h A" . cider-apropos)
                  ("C-h D" . cider-apropos-documentation))
  :config 
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
  :bind (:augment company-mode
                  ("<tab>"   . company-indent-or-complete-common)
                  ("C-<tab>" . company-complete-common))
  :diminish company-mode
  :init
  (add-hook 'lisp-tag-hook #'company-mode)
  :config 
  (bind-keys :map company-active-map
             ("<tab>" . company-complete-common-or-cycle))
  (setq company-idle-delay nil))

(use-package dash
  :ensure t
  :defer t)

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
  :bind (:map aph-keys-mode-map
              ("C-z C-f" . elfeed))
  :bind (:augment elfeed-show-mode
                  ("M-p" . backward-paragraph)
                  ("M-n" . forward-paragraph))
  :config
  ;; Basic config
  (setq elfeed-sort-order            'ascending
        aph/elfeed-favorite-filters  '("@6-months-ago +unread" "+todo"))
  (setq url-queue-parallel-processes 1
        url-queue-timeout            30)
  (setq elfeed-db-directory "~/sync/elfeed")

  ;; Bugfixes
  ;; The various elfeed modes do not follow the convention of using
  ;; `run-mode-hooks' and thus do not run
  ;; `after-change-major-mode-hook'.  This can probably be fixed
  ;; through advice; in the meantime, the following line is an easy
  ;; stopgap fix for the immediate problem this causes for me.
  (add-hook 'elfeed-search-mode-hook #'aph-keys--update-major-mode)
  (add-hook 'elfeed-show-mode-hook   #'aph-keys--update-major-mode) 

  ;; Load the feed list
  (use-package init-elfeed))

(use-package aph-elfeed
  :after elfeed
  :bind (:augment elfeed-search-mode
                  ("<return>" . aph/elfeed-search-show-entry)
                  ("'"        . aph/elfeed-search-next-favorite-filter))
  :bind (:augment (elfeed-show-mode eww-mode)
                  ("p" . aph/elfeed-show-prev)
                  ("n" . aph/elfeed-show-next)))

(use-package ert
  :defer t
  :init 
  (add-to-list 'load-path (expand-file-name (concat aph/init-path "/test"))))

(use-package eww
  :bind (:map aph-keys-mode-map ("C-z C-w" . eww))
  :bind (:augment eww-mode
                  ("S-<tab>" . shr-previous-link)
                  ("["       . eww-previous-url)
                  ("]"       . eww-next-url)
                  ("z"       . shr-zoom-image)
                  ("M-p"     . backward-paragraph)
                  ("M-n"     . forward-paragraph)))

(use-package files
  :defer t
  :config
  ;; Backup file location
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups")))))

(use-package aph-files
  :after files
  :bind (:map aph-keys-mode-map
              ("C-x k"        . aph/kill-active-buffer)
              ("C-x C-c"      . aph/delete-frame-or-exit)
              ("C-x <delete>" . aph/kill-active-buffer-delete-file))
  :config
  ;; Make Emacs source read-only
  (when (eq aph/machine 'mpc)
    (setq aph/emacs-source-dirs
          '("C:/Program Files (Portable)/Emacs/share/emacs")))
  (aph/emacs-source-make-read-only))

(use-package find-func
  :bind (:map aph-keys-mode-map
              ("C-h C-M-f" . find-function)
              ("C-h C-M-k" . find-function-on-key)
              ("C-h C-M-v" . find-variable)
              ("C-h M-F"   . find-face-definition)
              ("C-h C-M-l" . find-library)))

(use-package font-lock
  :defer t
  :config
  (eval-after-load 'dash        #'dash-enable-font-lock))

(use-package aph-forms
  :after forms)

(use-package aph-frame
  :config
  ;; Machine-specific geometry
  (if (eq aph/machine 'mpc)
      (setq aph/frame-margin-width 16)
    (setq aph/frame-margin-width 4
          aph/frame-offset       -8)))

(use-package haskell-mode
  :ensure t
  :bind (:augment haskell-mode
                  ("C-c C-c" . haskell-compile)
                  ("C-c C-z" . haskell-interactive-switch))
  :config 
  ;; Stack setup
  (setq haskell-compile-cabal-build-command "stack build")
  (add-to-list 'process-coding-system-alist '("stack" . utf-8-dos))
  ;; REPL setup
  (setq haskell-process-show-debug-tips nil
        haskell-process-log t))

(use-package aph-haskell
  :after haskell-mode
  :bind (:augment haskell-mode
                  ("C-c C-l" . aph/haskell-process-load-file-cygwin)))

(use-package helm
  :ensure t
  :demand t
  :bind (:map aph-keys-mode-map
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
              ("C-z C-p"                . helm-list-elisp-packages)) 
  :bind (:map aph-keys-mode-helm-map
              ("<tab>"    . helm-execute-persistent-action)
              ("C-j"      . undefined)
              ("C-z"      . undefined)
              ("s-<apps>" . helm-select-action)))

(use-package aph-helm
  :bind (:map aph-keys-mode-map
              ("C-x M-p" . aph/helm-browse-project))
  :bind (:map aph-keys-mode-helm-map
              ("<return>" . aph/helm-resume-update-or-exit-minibuffer)))

(use-package helm-config
  :after helm
  :diminish helm-mode
  :bind (:map aph-keys-mode-map
              ("C-x c"     . undefined)
              ("C-x x"     . helm-command-prefix)
              ("C-x x C-u" . helm-resume))
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
  :defer t)

(use-package aph-helm-projectile
  :bind (:map aph-keys-mode-map
         ("C-x p"   . aph/helm-projectile)
         ("M-s M-g" . aph/helm-projectile-grep)))

(use-package help
  :bind (:map aph-keys-mode-map
              ("C-h C-h" . undefined)))

(use-package aph-help
  :bind (:map aph-keys-mode-map
              ("C-h h" . aph/call-logging-hooks)))

(use-package help+
  :ensure t
  :after help)

(use-package help-mode+
  :ensure t
  :after help-mode)

(use-package help-fns+
  :after help-fns
  :bind (:map aph-keys-mode-map
              ("C-h c"   . describe-key-briefly)
              ("C-h M-b" . describe-buffer)
              ("M-= b" . describe-buffer))
  :config
  ;; Add Org manual to list of manuals to include links for in help.
  ;; This is a stopgap measure.  In the long run, keying in to
  ;; `Info-file-list-for-emacs' is probably the way to go.
  (setq help-cross-reference-manuals '(("emacs" "elisp" "org"))))

(use-package hippie-exp
  :bind (:map aph-keys-mode-map
              ([remap dabbrev-expand] . hippie-expand))
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
  :bind (:map aph-keys-mode-map
              ("C-c h l" . hl-line-mode)))

(use-package ibuffer
  :bind (:map aph-keys-mode-map
              ([remap list-buffers] . ibuffer)))

(use-package ido
  :defer t
  :config
  (setq ido-auto-merge-work-directories-length -1
        ido-enable-flex-matching               t
        ido-use-filename-at-point              'guess)
  ;; If more than one extension appears on a line, it means that I
  ;; expect files of those extensions might appear together, and I care
  ;; which order they're sorted in.  Otherwise, I just care about
  ;; sorting relative to the t symbol, which represents files with
  ;; extensions not appearing in this list.
  (setq ido-file-extensions-order '(".ahk"
                                    ".clj"
                                    ".el"
                                    ".org"
                                    ".tex" ".pdf"
                                    ".txt"
                                    t
                                    ".log"))
  ;; Extensions to ignore (mostly byproducts of LaTeX compilation)
  (nconc completion-ignored-extensions
         '(".tex.swp" "_.log" ".prv/" "_.tex" ".rip")))

(use-package ielm
  :bind (:map aph-keys-mode-map
              ("C-z C-r" . ielm))
  :bind (:augment inferior-emacs-lisp-mode
                  ("C-c C-t" . aph/eval-expression-toggle-clean-output)
                  ("C-c M-w" . aph/ielm-copy-last-output))
  :config
  (use-package aph-ielm) 
  (aph/mode-tag-add 'ielm-mode 'lisp))

(use-package aph-iimage
  :after iimage
  :bind (:augment iimage-mode
                  ("C-c i" . aph/iimage-refresh)))

(use-package info
  :defer t
  :config
  ;; Improves functionality of `C-h F' for Org commands.
  (add-to-list 'Info-file-list-for-emacs "org"))

(use-package aph-info
  :after info
  :bind (:map aph-keys-mode-map
              ("C-h i" . aph/info-mode-or-clone-buffer))
  :bind (:augment Info-mode
                  ("0" . aph/Info-final-menu-item)))

(use-package aph-keypad
  :bind (:map aph-keys-mode-map 
              ("C-<kp-enter>" . aph/keypad-enter-toggle-newline)))

(use-package lisp-mode
  :bind (:augment emacs-lisp-mode
                  ("C-c e b" . eval-buffer)
                  ("C-c e d" . eval-defun)
                  ("C-c e r" . eval-region)
                  ("C-c e e" . eval-last-sexp))
  :config 
  ;; Mode tags
  (aph/mode-tag-add 'lisp-mode             'lisp)
  (aph/mode-tag-add 'emacs-lisp-mode       'lisp)
  (aph/mode-tag-add 'lisp-interaction-mode 'lisp)
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
                 1)))

(use-package minibuffer
  :bind (:map aph-keys-mode-map
              ("C-<tab>" . completion-at-point))
  :config
  (setq completion-auto-help 'lazy))

(use-package org
  :ensure t
  :bind (:map aph-keys-mode-map
              ("C-c l" . org-store-link)
              ("C-="   . org-increase-number-at-point))
  :bind (:augment org-mode
                  ("C-c ["            . undefined)
                  ("C-c ]"            . undefined)
                  ("M-."              . helm-org-in-buffer-headings)
                  ([remap next-error] . org-cycle-agenda-files)
                  ;; Following bindings restore `org-mode' keys
                  ;; unintentionally shadowed by `aph-keys-mode'
                  ("C-o"              . org-open-line))
  :config
  (message "Loading org...")         ; Because this may take a while. 
  (use-package init-org))

(use-package aph-org
  :after org
  :bind (:map aph-keys-mode-map
              ("C-c w" . aph/org-goto-last-refile))
  :bind (:augment org-mode
                  ("C-c s SPC"   . aph/org-spin-basic)
                  ("C-c s M-SPC" . aph/org-spin-weighted)))

(use-package org-agenda
  :defer t
  :bind (:augment org-agenda-mode
                  ;; `org-agenda-quit' is bugged, as of Emacs 24.5.1
                  ;; and Org 8.3.2.  This might be a bug in
                  ;; `bury-buffer'?  Anyway, `quit-window' still
                  ;; works, so let's just use that for the time being.
                  ([remap org-agenda-quit] . quit-window)
                  ("C-o"                   . org-agenda-open-link)))

(use-package aph-org-agenda
  :after org-agenda
  :bind (:map aph-keys-mode-map
              ("C-c a" . aph/org-agenda)
              ("<f1>" . aph/org-agenda-display-smart-agenda)))

(use-package org-capture
  :defer t
  :config
  (require 'init-org-capture))

(use-package aph-org-capture
  :after org-capture
  :bind (:map aph-keys-mode-map
              ("C-c c" . aph/org-capture-in-popout-frame))
  :config
  ;; Support for `aph/org-capture-in-popout-frame':
  (add-hook 'org-capture-after-finalize-hook
            #'aph/org-capture-delete-capture-frame))

(use-package org-clock
  :bind (:map aph-keys-mode-map
              ("C-c t j" . org-clock-goto)
              ("C-c t o" . org-clock-out)
              ("C-c t x" . org-clock-cancel)
              ("C-c t r" . org-clock-in-last))
  :bind (:augment org-mode
                  ("C-c t i" . org-clock-in)))

(use-package org-mobile
  :disabled t
  :config
  (setq org-mobile-directory      "~/sync/mobile"
        org-mobile-inbox-for-pull (concat org-directory "/capture.org"))
  (when (eq aph/machine 'mpc)
    (setq org-mobile-checksum-binary
          "C:/Program Files (Portable)/GnuWin Core Utilities/bin/sha1sum.exe")))

(use-package outline
  :bind (:override outline-mode
                   ("C-M-b" . outline-backward-same-level)
                   ("C-M-f" . outline-forward-same-level)
                   ("C-M-n" . outline-next-visible-heading)
                   ("C-M-p" . outline-previous-visible-heading)
                   ("C-M-u" . outline-up-heading)))

(use-package aph-outline
  :bind (:override outline-mode
                   ("C-M-d" . aph/outline-down-heading-from-end)))

(use-package page 
  :config
  (put 'narrow-to-page 'disabled nil))

(use-package aph-page
  :commands (aph/hydra-page/forward-page
             aph/hydra-page/backward-page)
  :init
  (bind-keys :map aph-keys-mode-map
              ([remap forward-page]  . aph/hydra-page/forward-page)
              ([remap backward-page] . aph/hydra-page/backward-page)))

(use-package paren
  :config
  (show-paren-mode))

(use-package pp
  :bind (:map aph-keys-mode-map
              ("C-:" . pp-eval-expression)))

(use-package prog-mode
  :bind (:augment prog-mode
                  ("M-p" . backward-paragraph)
                  ("M-n" . forward-paragraph)))

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
  :bind (:map aph-keys-mode-map
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
  :bind (:map aph-keys-mode-map
              ("C-M-y" . aph/yank-rectangle-from-kill-ring))
  :config
  (when (>= emacs-major-version 25)
    (advice-add #'rectangle--*-char :around #'aph/rectangle-repetition-fix)))

(use-package register
  :bind (:map aph-keys-mode-map
              ("C-m"     . copy-to-register) 
              ("C-M-m"   . increment-register)))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package server
  :disabled t
  :config
  (unless (server-running-p) (server-start))
  (setq server-window 'pop-to-buffer))

(use-package shm
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'structured-haskell-mode)
  :config
  (add-hook 'structured-haskell-mode-hook
            #'aph/haskell-indentation-mode:off)
  (add-hook 'structured-haskell-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'structured-haskell-mode-hook #'electric-indent-local-mode))

(use-package shr
  :defer t
  :config
  (use-package aph-shr)
  (advice-add #'shr-urlify :before #'aph/shr-urlify-advice))

(use-package simple
  :demand t
  :bind (:map aph-keys-mode-map
              ("M-o"        . join-line)
              ("C-M-/"      . undo-only)
              ("C-S-k"      . kill-whole-line)
              ("C-x M-k"    . append-next-kill)
              ("S-<return>" . delete-blank-lines)
              ("S-SPC"      . cycle-spacing)
              ("M-= w"      . count-words)
              ("M-SPC"      . mark-word)
              ("M-= l"      . what-line))
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
  :bind (:map aph-keys-mode-map
              ("C-a"             . aph/move-beginning-of-line)
              ([remap open-line] . aph/open-line)
              ("M-c"             . aph/hydra-caps/body)
              ("M-l"             . undefined)
              ("M-u"             . undefined)
              ("C-`"             . next-error)
              ("M-`"             . previous-error))
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

(use-package smartparens
  :ensure t 
  :bind (:augment smartparens-mode
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
                  ("M-u SPC"                . sp-prefix-save-excursion)
                  ("M-u '"                  . sp-prefix-symbol-object)
                  ("M-u ("                  . sp-prefix-pair-object)
                  ("M-u ["                  . sp-prefix-pair-object)
                  ("M-u ,"                  . sp-prefix-tag-object))
  :bind (:augment smartparens-strict-mode
                  (")" . sp-up-sexp)
                  ("]" . sp-up-sexp)
                  ("}" . sp-up-sexp))
  :init
  (require 'smartparens-config)
  (smartparens-global-mode)
  (add-hook 'lisp-tag-hook #'smartparens-strict-mode)
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

(use-package smartscan
  :ensure t
  :bind (:augment smartscan-mode 
                  ("C-M-r" . smartscan-symbol-go-backward)
                  ("C-M-s" . smartscan-symbol-go-forward)
                  ("C-M-'" . smartscan-symbol-replace))
  :init
  (add-hook 'text-mode-hook #'smartscan-mode)
  (add-hook 'prog-mode-hook #'smartscan-mode))

(use-package solar
  :defer t
  :config
  (setq calendar-longitude -93.2
        calendar-latitude   45.0))

(use-package sort
  :bind (:map aph-keys-mode-map
              ("s-<apps> d" . delete-duplicate-lines)
              ("s-<apps> s" . sort-lines)))

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

(use-package text-mode
  :bind (:augment text-mode
                  ("M-p" . backward-paragraph)
                  ("M-n" . forward-paragraph)))

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
  :bind (:map aph-keys-mode-map
              ;; Scrolling and Positioning
              ("M-l"   . move-to-window-line-top-bottom)
              ("C-M-v" . aph/hydra-scroll-other/body)
              ;; Sliding windows
              ("<C-[>" . aph/other-window-backward)
              ("M-["   . aph/pull-buffer-backward)
              ("M-]"   . aph/slide-buffer-forward)
              ("C-\\"  . aph/swap-buffer-forward-and-ride)
              ("M-\\"  . aph/swap-buffer-forward)
              ;; Other commands
              ("C-c q" . aph/quit-help-windows)))

(use-package winner
  :config
  (winner-mode))

(use-package xahk-mode
  :mode "\\.ahk\\'") 

(provide 'init-core)
(provide 'init)
