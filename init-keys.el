;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

(require 'aph-require)                  ; For `aph/require-softly'
(require 'aph-keys)                     ; For `aph/define-keys-safely', etc. 


;;; Other Keybindings
;;;==================
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
