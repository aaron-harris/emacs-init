;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

(require 'aph-require)                  ; For `aph/require-softly'
(require 'aph-keys)                     ; For `aph/define-keys-safely', etc. 


;;; Other Keybindings
;;;==================
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
