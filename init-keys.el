;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

(require 'aph-require)                  ; For `aph/require-softly'
(require 'aph-keys)                     ; For `aph/define-keys-safely', etc. 


;;; Programming Keybindings
;;;========================
;; Elisp 
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
