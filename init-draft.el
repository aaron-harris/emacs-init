;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.


;;; Augmented Keymaps: Front End
;;;=============================
;; End goal forms.  These should all do the same thing, more or less.
(define-key (aph-keys-augment 'org-mode) (kbd "C-a") #'ignore)
(bind-key "C-a" #'ignore (aph-keys-augment 'org-mode))
(bind-keys :augment org-mode
           ("C-a" . ignore))

;; Current status: `define-key' works, but we need more work for
;; `bind-key' and `bind-keys'.


;;; Augmented Keymaps: `bind-key' Support
;;;======================================
;; TODO: Make `describe-personal-keybindings' work properly with this.
;;   This will probably involve advice to `bind-key' to process the
;;   form added to `personal-keybindings'.


;;; Augmented Keymaps: `bind-keys' Support
;;;=======================================
(defun aph/bind-keys-augment-handler (&rest args)
  "Process :augment key in ARGS for `bind-keys'.
This function is intended as :filter-args advice for the macro
`bind-keys'.  It adds support for the :augment keyword argument.

To use this argument, supply the name of a major or minor mode
keymap.  This is augmented using `aph-keys-augment' and the keys
are bound to the augmented map."
  (let ((mode  (plist-get args :augment)))
    (if mode
        ;; Note that we aren't removing the :augment key here.
        ;; Instead we're relying on undocumented behavior of
        ;; `bind-keys', specifically that it simply ignores keywords
        ;; it does not recognize.
        `(:map ,(aph-keys-augment-var mode) ,@args)
      args)))

(advice-add #'bind-keys :filter-args #'aph/bind-keys-augment-handler)
(advice-remove #'bind-keys #'aph/bind-keys-augment-handler)

;; TODO: Write test for this advice.


(provide 'init-draft)
