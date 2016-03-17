;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; BIND-KEY EXTENSIONS
;;;;============================================================================

;; Extensions for the `bind-key' package.

(require 'bind-key)


;;; Fixes for `describe-personal-keybindings'
;;;==========================================
;; It is possible to use the `bind-keys' macro in several different
;; ways that are all legitimate, but cause subsequent calls to
;; `describe-personal-keybindings' to signal an error for one reason
;; or another.  Functions in this section are attempts to fix these
;; kinds of errors.

(defun aph/bind-key-get-binding-description-advice--menu-item (elem)
  "Advice to let `get-binding-description' handle menu items.

If the `bind-key' macro is passed a menu item as a command, a
subsequent call to `describe-personal-keybindings' will signal an
error because the function `get-binding-description' doesn't know
how to handle a menu item.  This function fixes that; it is
intended as :before-until advice on `get-binding-description'.

The description returned is the item name associted with the menu
item; e.g., for the menu item

  (menu-item \"foo\" nil
    :filter (can-foo-p))

the return value is \"foo\"."
  (when (and (listp elem)
             (eq 'menu-item (car elem)))
    (nth 1 elem)))

(advice-add 'get-binding-description :before-until
            #'aph/bind-key-get-binding-description-advice--menu-item)

(provide 'aph-bind-key)
