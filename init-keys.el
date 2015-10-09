;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

(require 'aph-require)                  ; For `aph/require-softly'
(require 'aph-keys)                     ; For `aph/define-keys-safely', etc.


;;; Modifier Key Setup
;;;===================
;; Use Windows keys for super.
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super)


;;; General Keybindings
;;;====================
;; Removing Unnecessary Bindings
(global-unset-key (kbd "C-z"))          ; I don't need minimization on a key.
(global-unset-key (kbd "M-="))          ; Freeing this for use as a prefix.
(global-unset-key (kbd "C-h a"))        ; Freeing this for use as a prefix.
(global-unset-key (kbd "M-!"))          ; Freeing this for use as a prefix.

(aph/global-set-keys-safely
  ;; Scrolling
  ((kbd "<down>")         #'aph/scroll-up-by-line   :rebind)
  ((kbd "<up>")           #'aph/scroll-down-by-line :rebind)
  ;; Basic Editing
  ([remap open-line]      #'aph/open-line)
  ((kbd "C-S-o")          #'join-line)
  ;; Undo
  ((kbd "C-M-/")          #'undo-only :rebind)
  ;; Kill and Copy
  ((kbd "C-S-k")          #'kill-whole-line)  ; Was at C-S-<backspace>
  ((kbd "C-x M-k")        #'append-next-kill) ; Was at C-M-w
  ;; Completion
  ([remap dabbrev-expand] #'hippie-expand)
  ;; Cyclic Window Control
  ((kbd "s-]")            #'other-window)
  ((kbd "s-[")            #'aph/other-window-backwards)
  ((kbd "s-{")            #'aph/pull-buffer-backward)
  ((kbd "s-}")            #'aph/slide-buffer-forward)
  ((kbd "s-\\")           #'aph/swap-buffer-forward-and-ride)
  ((kbd "s-|")            #'aph/swap-buffer-forward)
  ;; General Buffer Control
  ([remap list-buffers]   #'ibuffer)
  ((kbd "C-x k")          #'aph/kill-active-buffer :rebind)
  ;; Window and Frame Control
  ((kbd "C-c q")          #'aph/quit-help-windows)
  ((kbd "C-x C-c")        #'aph/delete-frame-or-exit :rebind)
  ;; Information about Buffer
  ((kbd "M-= w")          #'count-words)
  ((kbd "M-= l")          #'what-line)
  ((kbd "M-= p")          #'aph/sum-parens-in-region)
  ;; Buffer/Region Manipulation
  ((kbd "s-<apps> d")     #'delete-duplicate-lines)
  ;; Application Control
  ((kbd "C-c C-o")        #'browse-url)
  ;; Shell Commands
  ((kbd "M-! M-!")        #'shell-command)
  ((kbd "M-! M-&")        #'async-shell-command)
  ((kbd "M-! r")          #'shell-command-on-region)
  ;; Highlighting
  ((kbd "C-c h l")        #'hl-line-mode)
  ;((kbd "C-c h x")       #'flash-crosshairs)
  ;; Themes
  ((kbd "s-n")            #'aph/theme-cycle)
  ;; Keybinding Control
  ((kbd "C-<kp-enter>")   #'aph/kp-enter-newline-toggle)
  ;; Apropos
  ((kbd "C-h a a")        #'apropos)
  ((kbd "C-h a c")        #'apropos-command)
  ((kbd "C-h a f")        #'aph/apropos-function)
  ((kbd "C-h a v")        #'apropos-variable)
  ((kbd "C-h a <space>")  #'apropos-value)
  ((kbd "C-h a d")        #'apropos-documentation)
  ((kbd "C-h a l")        #'apropos-library)
  ((kbd "C-h a i")        #'info-apropos)
  ((kbd "C-h a t")        #'tags-apropos))


;;; Smartparens Keybindings
;;;========================
(with-eval-after-load 'smartparens 
  (aph/define-keys-safely smartparens-mode-map
    ;; Movement
    ([remap backward-sexp]    #'sp-backward-sexp)
    ([remap forward-sexp]     #'sp-forward-sexp)
    ([remap backward-up-list] #'sp-backward-up-sexp)
    ([remap down-list]        #'sp-down-sexp)
    ([remap forward-list]     #'sp-next-sexp)
    ([remap backward-list]    #'sp-previous-sexp)
    ((kbd "M-B")              #'sp-backward-symbol)
    ((kbd "M-F")              #'sp-forward-symbol)
    ;; Selection
    ([remap mark-sexp]        #'sp-select-next-thing-exchange)
    ;; Barf and Slurp
    ((kbd "C-<left>")         #'sp-forward-barf-sexp)
    ((kbd "C-<right>")        #'sp-forward-slurp-sexp)
    ((kbd "C-S-<left>")       #'sp-backward-slurp-sexp)
    ((kbd "C-S-<right>")      #'sp-backward-barf-sexp)
    ;; Kill and Copy
    ([remap kill-sexp]        #'sp-kill-sexp)
    ((kbd "C-M-w")            #'sp-copy-sexp)
    ((kbd "C-S-<backspace>")  #'sp-splice-sexp-killing-around)
    ;; Editing
    ([remap transpose-sexps]  #'sp-transpose-sexp)
    ;; Unwrap and Splice
    ((kbd "C-<delete>")       #'sp-unwrap-sexp)
    ((kbd "C-<backspace>")    #'sp-backward-unwrap-sexp)
    ((kbd "M-D")              #'sp-splice-sexp)
    ;; Indentation
    ((kbd "M-)")              #'sp-up-sexp)
    ;; Narrowing
    ((kbd "C-x n (")          #'sp-narrow-to-sexp))
  (aph/define-keys-safely smartparens-strict-mode-map
    ((kbd ")")                #'sp-up-sexp)))


;;; Org Mode Keybindings
;;;=====================
(aph/global-set-keys-safely
  ;; Agenda Commands
  ((kbd "C-c a")   #'aph/org-agenda)
  ((kbd "<f1>")    #'aph/org-agenda-display-smart-agenda :rebind)
  ;; Capture and Refile Commands
  ((kbd "C-c c")   #'aph/org-capture-in-popout-frame)
  ;; Link Commands
  ((kbd "C-c l")   #'org-store-link))
(with-eval-after-load 'org 
  (aph/global-set-keys-safely
    ;; Capture and Refile Commands
    ((kbd "C-c w")   #'aph/org-goto-last-refile)
    ;; Clocking Commands
    ((kbd "C-c t j") #'org-clock-goto)
    ((kbd "C-c t o") #'org-clock-out)
    ((kbd "C-c t x") #'org-clock-cancel)
    ((kbd "C-c t r") #'org-clock-in-last))
  (aph/define-keys-safely org-mode-map
    ;; Unbinding
    ((kbd "C-c [")   nil :rebind)
    ((kbd "C-c ]")   nil :rebind)
    ;; Clocking Commands
    ((kbd "C-c t i") #'org-clock-in)
    ;; Spinner Commands
    ((kbd "C-z s")   #'aph/org-spin-basic)
    ((kbd "C-z C-s") #'aph/org-spin-weighted)))


;;; Programming Keybindings
;;;========================
;; Elisp
(aph/global-set-keys-safely
  ((kbd "C-x M-l") #'find-library)
  ((kbd "C-:")     #'pp-eval-expression))

(with-eval-after-load 'ielm
  (aph/define-keys-safely ielm-map
    ((kbd "C-c M-w") #'aph/ielm-copy-last-output)
    ((kbd "C-c C-t") #'aph/eval-expression-toggle-clean-output)))

;; Clojure and Cider
(aph/global-set-key-safely (kbd "C-c M-c") #'cider-connect)
(with-eval-after-load 'cider
  (aph/define-keys-safely clojure-mode-map
    ((kbd "C-h A a") #'cider-apropos)
    ((kbd "C-h A d") #'cider-apropos-documentation)))


;;; Other Keybindings
;;;==================
;; Avy
(aph/global-set-keys-safely
  ((kbd "M-g M-q") #'avy-goto-char-2)
  ((kbd "M-g q")   #'avy-goto-char)
  ((kbd "M-g M-g") #'avy-goto-line :rebind)
  ((kbd "M-g M-w") #'avy-goto-word-or-subword-1))
(aph/define-keys-safely isearch-mode-map 
  ((kbd "M-g")     #'avy-isearch))

;; Company Mode
(with-eval-after-load 'company
  (aph/define-keys-safely company-mode-map
    ((kbd "<tab>") #'company-indent-or-complete-common))
  (aph/define-keys-safely company-active-map
    ((kbd "<tab>") #'company-complete-common-or-cycle :rebind)))

;; Elfeed
(with-eval-after-load 'elfeed
  (aph/define-keys-safely elfeed-search-mode-map
    ((kbd "<return>") #'aph/elfeed-search-show-entry :rebind))
  (aph/define-keys-safely elfeed-show-mode-map
    ((kbd "p")        #'aph/elfeed-show-prev :rebind)
    ((kbd "n")        #'aph/elfeed-show-next :rebind)))

;; Eww
(with-eval-after-load 'eww
  (aph/define-keys-safely eww-mode-map
    ;; Clear keys for later rebinding in presence of `elfeed'
    ((kbd "p")       nil :rebind)
    ((kbd "n")       nil :rebind)
    ;; Intrapage navigation
    ((kbd "S-<tab>") #'shr-previous-link)
    ;; Interpage navigation
    ((kbd "[")       #'eww-previous-url) ; Moved from p
    ((kbd "]")       #'eww-next-url))    ; Moved from n
  (with-eval-after-load 'elfeed
    (aph/define-keys-safely eww-mode-map
      ((kbd "p")     #'aph/elfeed-show-prev)
      ((kbd "n")     #'aph/elfeed-show-next))))

;; Info
(aph/global-set-keys-safely
  ((kbd "C-h i") #'aph/info-mode-or-clone-buffer :rebind))
(with-eval-after-load 'info
  (aph/define-keys-safely Info-mode-map
    ((kbd "0") #'aph/Info-final-menu-item :rebind)))

;; Mercurial
(aph/global-set-keys-safely
 ((kbd "M-! h s") #'aph/hg-status)
 ((kbd "M-! h l") #'aph/hg-log)
 ((kbd "M-! h c") #'aph/hg-commit))


;;; Machine-Specific Keybindings
;;;=============================
(when (eq aph/machine 'mpc)
  (aph/global-set-key-safely (kbd "C-x C-y") #'aph/yank-access-inline))

(provide 'init-keys)
