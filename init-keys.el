;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

(require 'aph-require)                  ; For `aph/require-softly'
(require 'aph-keys)                     ; For `aph/define-keys-safely', etc. 


;;; General Keybindings
;;;==================== 
(aph/global-set-keys-safely 
  ;; Themes
  ((kbd "s-n")             #'aph/theme-cycle)
  ;; Keybinding Control
  ((kbd "C-<kp-enter>")    #'aph/kp-enter-newline-toggle)
  ;; Help Keys
  ((kbd "C-h C-h")         nil :rebind) ; `help-for-help'
  ((kbd "C-h c")           #'describe-key-briefly :rebind)
  ((kbd "C-h M-b")         #'describe-buffer))
(with-eval-after-load 'smartscan
  (aph/define-keys-safely smartscan-map
    ((kbd "M-n")           nil :rebind) ; `smartscan-symbol-go-forward'
    ((kbd "C-M-s")         #'smartscan-symbol-go-forward)
    ((kbd "M-p")           nil :rebind) ; `smartscan-symbol-go-backward'
    ((kbd "C-M-r")         #'smartscan-symbol-go-backward)))


;;; Helm Keybindings
;;;=================
(with-eval-after-load 'helm
  (aph/global-set-keys-safely
    ;; Helm
    ((kbd "C-x x")            #'helm-command-prefix)
    ((kbd "C-x c")            nil :rebind) ; `helm-command-prefix'
    ((kbd "C-x x C-u")        #'helm-resume)
    ;; Command Invocation
    ((kbd "M-x")              #'helm-M-x :rebind)
    ((kbd "C-M-:")            #'helm-eval-expression-with-eldoc) 
    ;; Yank
    ((kbd "M-y")              #'helm-show-kill-ring :rebind) ; `yank-pop'
    ;; Registers
    ((kbd "C-x r i")          #'helm-register :rebind) ; `insert-register'
    ;; Bookmarks
    ((kbd "C-c b b")          #'helm-filtered-bookmarks :rebind) ; `bookmark-jump'
    ;; Navigation and Search
    ((kbd "C-c ,")            #'helm-semantic-or-imenu)
    ((kbd "M-s o")            #'helm-occur :rebind) ; `occur'
    ((kbd "M-s r")            #'helm-regexp)
    ((kbd "C-c SPC")          #'helm-all-mark-rings)
    ;; General Buffer Control
    ([remap switch-to-buffer] #'helm-mini)
    ([remap find-file]        #'helm-find-files)
    ((kbd "C-x p")            #'helm-projectile)
    ((kbd "C-x M-p")          #'helm-browse-project)
    ;; Filesystem Interaction
    ((kbd "M-s g")            #'helm-do-grep)
    ((kbd "M-s M-g")          #'helm-projectile-grep)
    ;; Application Control
    ((kbd "C-z C-s")          #'helm-google-suggest)
    ;; Help Keys
    ((kbd "C-h C-f")          #'helm-colors :rebind) ; `view-emacs-FAQ'
    ([remap manual-entry]     #'helm-man-woman)
    ((kbd "C-h C-i")          #'helm-info-at-point)
    ((kbd "C-h a")            #'helm-apropos :rebind)) ; `apropos'
  (aph/define-keys-safely helm-map
    ;; Persistent Action
    ;; (Disabling C-j and C-z is so tooltip correctly points to <tab>.)
    ((kbd "<tab>")    #'helm-execute-persistent-action)
    ((kbd "C-j")      nil :rebind)      ; `helm-execute-persistent-action'
    ((kbd "C-z")      nil :rebind)      ; `helm-execute-persistent-action'
    ;; Action Menu
    ((kbd "s-<apps>") #'helm-select-action)))


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
  ((kbd "C-c a")         #'aph/org-agenda)
  ((kbd "<f1>")          #'aph/org-agenda-display-smart-agenda :rebind)
  ;; Capture and Refile Commands
  ((kbd "C-c c")         #'aph/org-capture-in-popout-frame)
  ;; Link Commands
  ((kbd "C-c l")         #'org-store-link))
(with-eval-after-load 'org
  (aph/global-set-keys-safely
    ;; Capture and Refile Commands
    ((kbd "C-c w")       #'aph/org-goto-last-refile)
    ;; Clocking Commands
    ((kbd "C-c t j")     #'org-clock-goto)
    ((kbd "C-c t o")     #'org-clock-out)
    ((kbd "C-c t x")     #'org-clock-cancel)
    ((kbd "C-c t r")     #'org-clock-in-last))
  (aph/define-keys-safely org-mode-map
    ;; Unbinding
    ((kbd "C-c [")       nil :rebind)
    ((kbd "C-c ]")       nil :rebind)
    ;; Buffer Navigation
    ((kbd "C-c C-j")     #'helm-semantic-or-imenu :rebind) ; `org-goto'
    ;; Clocking Commands
    ((kbd "C-c t i")     #'org-clock-in)
    ;; Spinner Commands
    ((kbd "C-c s SPC")   #'aph/org-spin-basic)
    ((kbd "C-c s M-SPC") #'aph/org-spin-weighted)))


;;; Programming Keybindings
;;;========================
;; Elisp
(aph/global-set-keys-safely
  ((kbd "C-x M-l")    #'find-library)
  ((kbd "C-:")        #'pp-eval-expression))
(with-eval-after-load 'ielm
  (aph/define-keys-safely ielm-map
    ((kbd "C-c M-w")  #'aph/ielm-copy-last-output)
    ((kbd "C-c C-t")  #'aph/eval-expression-toggle-clean-output)))

;; Clojure and Cider
(aph/global-set-keys-safely
  ((kbd "C-c M-c")  #'cider-connect))
(with-eval-after-load 'cider
  (aph/define-keys-safely cider-mode-map
    ((kbd "C-h A")  #'cider-apropos)
    ((kbd "C-h D")  #'cider-apropos-documentation)))


;;; Other Keybindings
;;;==================
;; Avy
(aph/global-set-keys-safely
  ((kbd "M-g M-q")  #'avy-goto-char-2)
  ((kbd "M-g q")    #'avy-goto-char)
  ((kbd "M-g M-g")  #'avy-goto-line :rebind) ; `goto-line'
  ((kbd "M-g M-w")  #'avy-goto-word-or-subword-1))
(aph/define-keys-safely isearch-mode-map
  ((kbd "M-g")      #'avy-isearch))

;; Calc
(aph/global-set-keys-safely
  ((kbd "C-z C-c")  #'calc)
  ((kbd "C-z M-c")  #'helm-calcul-expression))

;; Company Mode
(with-eval-after-load 'company
  (aph/define-keys-safely company-mode-map
    ((kbd "<tab>")  #'company-indent-or-complete-common))
  (aph/define-keys-safely company-active-map
    ((kbd "<tab>")  #'company-complete-common-or-cycle :rebind)))

;; Elfeed
(aph/global-set-keys-safely
    ((kbd "C-z C-f")   #'elfeed))
(with-eval-after-load 'elfeed
  (aph/define-keys-safely elfeed-search-mode-map
    ((kbd "<return>")  #'aph/elfeed-search-show-entry :rebind)
    ((kbd "'")         #'aph/elfeed-search-next-favorite-filter))
  (aph/define-keys-safely elfeed-show-mode-map
    ((kbd "p")         #'aph/elfeed-show-prev :rebind)
    ((kbd "n")         #'aph/elfeed-show-next :rebind)))

;; Eww
(aph/global-set-keys-safely
    ((kbd "C-z C-w")  #'eww))
(with-eval-after-load 'eww
  (aph/define-keys-safely eww-mode-map
    ;; Clear keys for later rebinding in presence of `elfeed'
    ((kbd "p")        nil :rebind) ; `eww-previous-url', see below
    ((kbd "n")        nil :rebind) ; `eww-next-url', see below
    ;; Intrapage navigation
    ((kbd "S-<tab>")  #'shr-previous-link)
    ;; Interpage navigation
    ((kbd "[")        #'eww-previous-url) ; Moved from p
    ((kbd "]")        #'eww-next-url)     ; Moved from n
    ;; Image commands
    ((kbd "z")        #'shr-zoom-image))
  (with-eval-after-load 'elfeed
    (aph/define-keys-safely eww-mode-map
      ((kbd "p")      #'aph/elfeed-show-prev)
      ((kbd "n")      #'aph/elfeed-show-next))))

;; Info
(aph/global-set-keys-safely
  ((kbd "C-h i")  #'aph/info-mode-or-clone-buffer :rebind)) ; `info'
(with-eval-after-load 'info
  (aph/define-keys-safely Info-mode-map
    ((kbd "0")    #'aph/Info-final-menu-item :rebind)))

;; Packages
(aph/global-set-keys-safely
  ((kbd "C-z C-p")  #'helm-list-elisp-packages))


;;; Machine-Specific Keybindings
;;;=============================
(when (eq aph/machine 'mpc)
  (aph/global-set-key-safely (kbd "C-x C-y") #'aph/yank-access-inline))

(provide 'init-keys)
