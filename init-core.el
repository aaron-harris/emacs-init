;;; init-core.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Aaron Harris
;; Author: Aaron Harris <meerwolf@gmail.com>

;;; Code:


;;; Bootstrapping Variables
;;;========================
;; These variables should have been set by the bootstrapper.
(defvar aph/sync-directory "~/sync"
  "The path to my Dropbox folder on the current machine.")

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


;;; Metaconfiguration Packages
;;;===========================
;; Packages in this section are loaded early so we can use them in
;; subsequent package declarations.

(use-package seq
  :ensure t
  :defer t
  :config
  (require 'seq-24))

(use-package validate
  :ensure t)

(use-package chimera)

(use-package hydra
  :ensure t
  :defer t
  :config
  (validate-setq lv-use-separator t))

(use-package mode-family
  :defer t)

(use-package umbra
  :bind (("C-x C-#" . umbra-mode))
  :bind (:map umbra-mode-map
              ("<return>" . umbra-default-return-command)
              ("<tab>"    . umbra-default-tab-command))
  :init (umbra-mode))


;;; Configuration: Source-Level
;;;============================
(prefer-coding-system 'utf-8-unix)
(validate-setq enable-recursive-minibuffers t)

;; Personal Information
(validate-setq user-full-name    "Aaron Harris")
(validate-setq user-mail-address "meerwolf@gmail.com")

;; UI Configuration
(validate-setq cursor-type                     'box)
(validate-setq frame-resize-pixelwise          t)
(validate-setq indent-tabs-mode                nil)
(validate-setq indicate-buffer-boundaries      'right)
(validate-setq inhibit-startup-screen          t)
(setq resize-mini-windows                      t) ;; Won't validate on Emacs 25
(validate-setq ring-bell-function              #'ignore)
(validate-setq scroll-conservatively           1000)
(validate-setq scroll-margin                   1)
(validate-setq scroll-preserve-screen-position :always)

;; Use Windows keys for super modifier.
(validate-setq w32-lwindow-modifier       'super)
(validate-setq w32-rwindow-modifier       'super)
(validate-setq w32-pass-lwindow-to-system nil)
(validate-setq w32-pass-rwindow-to-system nil)

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
           ("C-'"            . query-replace)
           ("M-'"            . query-replace-regexp) 
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


;;; Package Configuration
;;;======================
(use-package advice
  :defer t
  :config (validate-setq ad-redefinition-action 'accept))

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

(use-package aph-theme
  :defer t
  :init
  (add-to-list
   'custom-safe-themes
   "b50769e9fcc86519e961fe7614b5577368701c392afbb8604d5f1b07e2b5b7ac")
  (validate-variable 'custom-safe-themes))

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
  (validate-setq avy-all-windows     nil)
  (validate-setq avy-background      t)
  (validate-setq avy-highlight-first t)
  (validate-setq avy-style           'pre))

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

(use-package browse-url
  :bind (:map umbra-mode-map
              ("C-z <return>" . browse-url)))

(use-package browse-url-prefix
  :after browse-url
  :config
  (validate-setq browse-url-browser-function                #'browse-url-prefix)
  (validate-setq browse-url-prefix-default-browser-function #'eww-browse-url))

(use-package calc
  :bind (:map umbra-mode-map
              ("C-z C-c" . calc)))

(use-package cde
  :if (eq aph/machine 'mpc)
  :bind (:map umbra-mode-map
              ("C-c C-="   . cde-format)))

(use-package cider
  :ensure t
  :bind (:umbra cider-mode
                ("C-h A" . cider-apropos)
                ("C-h D" . cider-apropos-documentation))
  :config 
  (validate-setq cider-auto-select-error-buffer      nil)
  (validate-setq cider-repl-pop-to-buffer-on-connect nil)
  (validate-setq cider-show-error-buffer             'except-in-repl)
  (mode-family-add 'cider-repl-mode 'lisp)
  (mode-family-add 'cider-repl-mode 'clojure)
  (add-to-list 'aph/help-window-names "*cider-doc*")
  ;; Output from the JVM has Windows-style newlines, so we need to
  ;; strip those unless we want to see ^M characters in Cider buffers.
  (require 'cygwinize)
  (add-hook 'cider-repl-mode-hook            #'cygwinize-hide-dos-eol)
  (add-hook 'cider-macroexpansion-mode-hook  #'cygwinize-hide-dos-eol)
  (add-hook 'cider-test-report-mode-hook     #'cygwinize-hide-dos-eol))

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (mode-family-add 'clojure-mode 'lisp)
  (mode-family-add 'clojure-mode 'clojure)
  (add-hook 'clojure-family-hook #'subword-mode))

(use-package color-identifiers-mode
  :ensure t
  :defer t
  :diminish color-identifiers-mode
  :init
  (add-hook 'lisp-family-hook #'color-identifiers-mode)
  :config
  (validate-setq color-identifiers:num-colors      12)
  (validate-setq color-identifiers:color-luminance 0.65)

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

(use-package dash
  :ensure t
  :defer t)

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 200)
  ;; On mpc, Ghostview has a different name.
  (when (eq aph/machine 'mpc)
    (validate-setq doc-view-ghostscript-program "mgs.exe")))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'lisp-family-hook #'eldoc-mode))

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
  ;; UI config
  (validate-setq elfeed-sort-order 'ascending)

  ;; Network config
  (validate-setq url-queue-parallel-processes 1)
  (validate-setq url-queue-timeout            30)

  ;; Filesystem config
  (validate-setq elfeed-db-directory "~/sync/elfeed")

  ;; The various elfeed modes do not follow the convention of using
  ;; `run-mode-hooks' and thus do not run
  ;; `after-change-major-mode-hook'.  This can probably be fixed
  ;; through advice; in the meantime, the following line is an easy
  ;; stopgap fix for the immediate problem this causes for me.
  (add-hook 'elfeed-search-mode-hook #'umbra--update-major-mode)
  (add-hook 'elfeed-show-mode-hook   #'umbra--update-major-mode) 

  ;; Load the feed list
  (load (expand-file-name (concat elfeed-db-directory "/feeds.el")))
  (validate-variable 'elfeed-feeds))

(use-package elfeed-lens
  :bind (:umbra elfeed-search-mode
                ("'" . elfeed-lens-cycle)) 
  :config
  (validate-setq elfeed-lens-list '("@6-months-ago +unread" "+todo")))

(use-package elfeed-link
  :after elfeed
  :bind (:umbra eww-mode
                ("p" . elfeed-show-prev)
                ("n" . elfeed-show-next))
  :config
  (validate-setq elfeed-link-tag              'link)
  (validate-setq elfeed-link-browser-function #'eww-browse-url))

(use-package enumerate
  :bind (:map umbra-mode-map
              ("s-<apps> n"   . enumerate-lines)
              ("s-<apps> C-n" . enumerate-alpha)))

(use-package ert
  :bind (:umbra emacs-lisp-mode
                ("C-c C-t" . ert))
  :init 
  (add-to-list 'load-path (expand-file-name (concat aph/sync-directory
                                                    "/emacs/init/test")))
  (validate-variable 'load-path))

(use-package eww
  :bind (:map umbra-mode-map ("C-z C-w" . eww))
  :bind (:umbra eww-mode
                ("S-<tab>" . shr-previous-link)
                ("["       . eww-previous-url)
                ("]"       . eww-next-url)
                ("z"       . shr-zoom-image)
                ("M-p"     . backward-paragraph)
                ("M-n"     . forward-paragraph)))

(use-package expand-region
  :bind (:map umbra-mode-map ("C-;" . er/expand-region))
  :config
  (validate-setq expand-region-contract-fast-key "'")
  (validate-setq expand-region-reset-fast-key    " "))

(use-package files
  :defer t
  :config 
  (validate-setq backup-directory-alist
                 `(("." . ,(concat user-emacs-directory "backups"))))
  (validate-setq confirm-kill-emacs #'y-or-n-p))

(use-package aph-files
  :after files
  :bind (:map umbra-mode-map
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
  :bind (:map umbra-mode-map
              ("C-h C-M-f" . find-function)
              ("C-h C-M-k" . find-function-on-key)
              ("C-h C-M-v" . find-variable)
              ("C-h M-F"   . find-face-definition)
              ("C-h C-M-l" . find-library)))

(use-package font-lock
  :defer t
  :config
  (with-eval-after-load 'dash
    (dash-enable-font-lock)))

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
  :bind (:umbra haskell-mode
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
  :bind (:umbra haskell-mode
                ("C-c C-l" . aph/haskell-process-load-file-cygwin)))

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

(use-package aph-helm
  :bind (:map umbra-mode-map
              ("C-x M-p" . aph/helm-browse-project))
  :bind (:map umbra-mode-helm-map
              ("<return>" . aph/helm-resume-update-or-exit-minibuffer)))

(use-package helm-config
  :after helm
  :diminish helm-mode
  :bind (:map umbra-mode-map
              ("C-x c"     . undefined)
              ("C-x x"     . helm-command-prefix))
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
  :bind (:map umbra-mode-map
              ("C-x p"   . aph/helm-projectile)
              ("M-s M-g" . aph/helm-projectile-grep)))

(use-package help
  :bind (:map umbra-mode-map
              ("C-h C-h" . undefined)))

(use-package aph-help
  :after help
  :bind (:map umbra-mode-map
              ("C-h h" . aph/call-logging-hooks))
  :config
  (setq aph/help-mode-confirm-reversion nil))

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
  (setq help-cross-reference-manuals '(("emacs" "elisp" "org"))))

(use-package hippie-exp
  :bind (:map umbra-mode-map
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
  (add-hook 'lisp-family-hook #'aph/hippie-expand-config-lisp))

(use-package hl-line
  :bind (:map umbra-mode-map
              ("C-c h l" . hl-line-mode)))

(use-package ibuffer
  :bind (:map umbra-mode-map
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
  :bind (:map umbra-mode-map
              ("C-z C-r" . ielm))
  :bind (:umbra inferior-emacs-lisp-mode
                ("C-c C-t" . aph/eval-expression-toggle-clean-output)
                ("C-c M-w" . aph/ielm-copy-last-output))
  :config
  (use-package aph-ielm) 
  (mode-family-add 'ielm-mode 'lisp))

(use-package aph-iimage
  :after iimage
  :bind (:umbra iimage-mode
                ("C-c i" . aph/iimage-refresh)))

(use-package info
  :defer t
  :config
  ;; Improves functionality of `C-h F' for Org commands.
  (add-to-list 'Info-file-list-for-emacs "org"))

(use-package aph-info
  :after info
  :bind (:map umbra-mode-map
              ("C-h i" . aph/info-mode-or-clone-buffer))
  :bind (:umbra Info-mode
                ("0" . aph/Info-final-menu-item)))

(use-package jerk
  :if (eq aph/machine 'mpc)
  :bind (:map umbra-mode-map
              ("C-x C-y" . jerk-access-inline)))

(use-package aph-keypad
  :bind (:map umbra-mode-map 
              ("C-<kp-enter>" . aph/keypad-enter-toggle-newline)))

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

(use-package lisp-mode 
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
                 1)))

(use-package aph-lisp-mode
  :bind (:umbra emacs-lisp-mode 
                ("C-c C-c" . aph/eval-region-or-buffer)))

(use-package minibuffer
  :bind (:map umbra-mode-map
              ("C-<tab>" . completion-at-point))
  :config
  (setq completion-auto-help 'lazy))

(use-package multitheme
  :bind (:map umbra-mode-map
              ("s-n" . multitheme-cycle))
  :demand t
  :config
  (setq multitheme-base-theme-list '(hc-zenburn zenburn)
        multitheme-overtheme       'aph)
  (add-hook 'after-init-hook #'multitheme-cycle))

(use-package ob-core
  :defer t
  :config
  (setq org-confirm-babel-evaluate nil))

(use-package org
  :ensure t
  :bind (:map umbra-mode-map
              ("C-c l" . org-store-link))
  :bind (:umbra org-mode
                ("C-c ["            . undefined)
                ("C-c ]"            . undefined)
                ("M-."              . helm-org-in-buffer-headings)
                ([remap next-error] . org-cycle-agenda-files)
                ;; Following bindings restore `org-mode' keys
                ;; unintentionally shadowed by `umbra-mode'
                ("C-o"              . org-open-line))
  :bind (:penumbra org-mode
                   ("C-M-[" . org-metaleft)
                   ("C-M-]" . org-metaright)
                   ("C-M-t" . org-metaup))
  :config
  (message "Loading org...")         ; Because this may take a while. 
  (use-package init-org)
  (bind-keys
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
                ("C-k"         . aph/org-kill-line)
                ("C-c s SPC"   . aph/org-spin-basic)
                ("C-c s M-SPC" . aph/org-spin-weighted)))

(use-package org-agenda
  :defer t
  :bind (:umbra org-agenda-mode
                ;; `org-agenda-quit' is bugged, as of Emacs 24.5.1
                ;; and Org 8.3.2.  This might be a bug in
                ;; `bury-buffer'?  Anyway, `quit-window' still
                ;; works, so let's just use that for the time being.
                ([remap org-agenda-quit] . quit-window)
                ("C-o"                   . org-agenda-open-link)
                ("M-p"                   . org-agenda-backward-block)
                ("M-n"                   . org-agenda-forward-block)))

(use-package aph-org-agenda
  :after org-agenda
  :bind (:map umbra-mode-map
              ("C-c a" . aph/org-agenda)
              ("<f1>" . aph/org-agenda-display-smart-agenda)))

(use-package org-capture
  :defer t
  :config
  (require 'init-org-capture))

(use-package aph-org-capture
  :after org-capture
  :bind (:map umbra-mode-map
              ("C-c c" . aph/org-capture-in-popout-frame))
  :config
  ;; Support for `aph/org-capture-in-popout-frame':
  (add-hook 'org-capture-after-finalize-hook
            #'aph/org-capture-delete-capture-frame))

(use-package org-clock
  :bind (:map umbra-mode-map
              ("C-c t j" . org-clock-goto)
              ("C-c t o" . org-clock-out)
              ("C-c t x" . org-clock-cancel)
              ("C-c t r" . org-clock-in-last))
  :bind (:umbra org-mode
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

(use-package pp
  :bind (:umbra emacs-lisp-mode
                ("C-c C-m" . pp-macroexpand-last-sexp))
  :bind (:map umbra-mode-map
              ("C-:"   . pp-eval-expression)
              ("C-M-:" . pp-macroexpand-expression)))

(use-package prog-mode
  :bind (:umbra prog-mode
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
              ("C-M-y" . aph/yank-rectangle-from-kill-ring))
  :config
  (when (>= emacs-major-version 25)
    (advice-add #'rectangle--*-char :around #'aph/rectangle-repetition-fix)))

(use-package register
  :bind (:map umbra-mode-map
              ("C-m"     . copy-to-register) 
              ("C-M-m"   . increment-register)))

(use-package s
  :ensure t
  :defer t)

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
  :bind (:map umbra-mode-map
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
  (defun mode-family-run-hook--lisp ()
    "Run the hook for the `lisp' mode family."
    (run-hooks 'lisp-family-hook))
  (add-hook 'eval-expression-minibuffer-setup-hook
            #'mode-family-run-hook--lisp)
  (setq eval-expression-print-length nil
        eval-expression-print-level  nil))

(use-package aph-simple
  :after simple
  :bind (:map umbra-mode-map
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
                ("M-u SPC"                . sp-prefix-save-excursion)
                ("M-u '"                  . sp-prefix-symbol-object)
                ("M-u ("                  . sp-prefix-pair-object)
                ("M-u ["                  . sp-prefix-pair-object)
                ("M-u ,"                  . sp-prefix-tag-object))
  :bind (:umbra smartparens-strict-mode
                (")" . sp-up-sexp)
                ("]" . sp-up-sexp)
                ("}" . sp-up-sexp))
  :init
  (require 'smartparens-config)
  (smartparens-global-mode)
  (add-hook 'lisp-family-hook #'smartparens-strict-mode)
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
  (setq calendar-longitude -93.2
        calendar-latitude   45.0))

(use-package sort
  :bind (:map umbra-mode-map
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
  :bind (:umbra text-mode
                ("M-p" . backward-paragraph)
                ("M-n" . forward-paragraph)))

(use-package tooltip
  :config
  (tooltip-mode -1))

(use-package uniquify
  :config
  ;; Snippet from http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))



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

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode t))

(use-package which-func
  :init
  (which-function-mode))

(use-package aph-which-func
  :after which-func
  :config
  (aph/which-func-use-header-line)
  (add-to-list 'which-func-functions #'aph/which-function-org))

(use-package whitespace
  :bind (:map umbra-mode-map
              ("s-<apps> w" . whitespace-cleanup)))

(use-package aph-window
  :bind (:map umbra-mode-map
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

(provide 'init-core)
(provide 'init)
;;; init-core.el ends here
