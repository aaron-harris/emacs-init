;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.


;;; Key Binding Overhaul
;;;=====================
(defvar aph-keys-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<tab>") #'aph/foo)
    (define-key m (kbd "C-i") #'aph/bar)
    m)
  "Keymap for `aph-keys-mode'.")

(setq aph-keys-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<C-[>") #'aph/bar) 
    m))

(define-minor-mode aph-keys-mode
  "Mode for the personal keybindings of Aaron Harris."
  :global  t
  :lighter " #")


(provide 'init-draft)
