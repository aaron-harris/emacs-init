;;; chimera.el --- Conditional keybindings           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience keybinding

;; Dependencies: none
;; Advised functions from other packages:
;;   bind-key: `bind-keys', `get-binding-description'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs has built-in functionality for context-sensitive keybindings,
;; but because this is tied into the menu system (which many of us
;; don't use, myself included), the syntax for using them is not
;; particularly intuitive.
;;
;; This module provides a macro, called `chimera', that produces a
;; menu item that is set up for use as a context-sensitive keybinding.
;; (Throughout the module, we refer to such things as "chimeras",
;; because the term has appropriate connotations for this usage;
;; because it sounds cooler than "menu item"; and as a nod to the
;; popular `hydra' package.)
;;
;;
;; To use the `chimera' macro, just use it in place of the command in
;; any keybinding function (e.g., `define-key').  The body of the
;; chimera will be evaluated whenever the keybinding is looked up and
;; the result (a command) will be used in place of the chimera.
;;
;; The principal advantage of a chimera over a dispatch function is
;; that the chimera can also can return nil.  In this case, the
;; binding "vanishes" and key lookup continues with bindings that
;; would otherwise be shadowed.  Thus the chimera doesn't have to know
;; anything about what it should do in its "default case"; it can just
;; punt to the ordinary system of keymap precedence.
;;
;; This is a two-edged sword, however, because the chimera will be
;; consulted every time the binding is looked up, not just when the
;; key is entered.  This means that the chimera can't be seen by
;; documentation commands (e.g., `describe-key'), and it might be
;; difficult to diagnose anomalous behavior in chimeras.
;;
;;
;; For users of the `bind-key' package, this module also provides
;; support for chimeras.  This support should be entirely transparent
;; (just use the `chimera' macro inside `bind-key' or `bind-keys', as
;; you would inside `define-key'), but because this is done using
;; advice it may conflict with other extensions to `bind-key'.
;; Nonetheless, every effort is made to make the advice as unintrusive
;; as possible to minimize these conflicts.
;;
;; One minor benefit to using `chimera' with `bind-key' is that the
;; name supplied for the chimera will be shown in
;; `describe-personal-keybindings'.  However, this will always be
;; accompanied by a (possibly erroneous) "now: " comment, because the
;; chimera is consulted for its current command as part of the
;; `describe-personal-keybindings' command.
;;
;;
;; This module was inspired by a blog post by Artur Malabarba:
;;   http://endlessparentheses.com/define-context-aware-keys-in-emacs.html

;;; Code:

(eval-when-compile 'cl-lib)

(defmacro chimera (name &rest body)
  "Expand into a chimera with NAME and BODY.

Here NAME is just a string (not a symbol!) identifying the
chimera.  If you are using `bind-key', it will show up in your
personal keybindings; otherwise, the name is not used.

The BODY of the chimera is evaluated whenever it is called, and
the value of the last form (which should be a command) is called
immediately.  Essentially the chimera binding \"becomes\" this
command.  If BODY returns nil, then the chimera becomes
\"invisible\" and does not shadow lower-priority bindings on the
same key.

Note that this does not \"define\" a chimera in the
usual sense of associating a value with a symbol, largely because
Emacs does not recognize as a command a symbol whose value is a
menu item.  This means that the typical use case for this macro
is inside a call to `define-key', such as:

    (define-key foo-mode-map (kbd \"C-c C-c\")
      (chimera \"chimera/foo\"
        (when can-foo-p #'foo)))"
  (declare (debug (stringp body))
           (indent 1))
  `'(menu-item ,name nil
               :filter (lambda (&optional _) ,@body)))

(defun chimera-p (form &optional raw)
  "Return non-nil if FORM is a chimera.
Forms considered to be chimeras are menu items and unexpanded
macro calls to `chimera'.  Note that this includes menu items not
constructed with the `chimera' macro.

If RAW is non-nil, return non-nil only for unexpanded chimeras."
  (cond
   ((not (listp form))                         nil)
   ((eq (car form) 'chimera)                   t)
   ((and (not raw) (eq (car form) 'menu-item)  t))
   (:else                                      nil)))


;;; `bind-key' support
;;;===================
(defun chimera--bind-keys-advice (args)
  "Advice to add `chimera' support to `bind-keys'.

Because `bind-keys' does not evaluate its arguments before
passing them to `bind-key', it needs a little help to make the
following usage valid:

    (bind-keys :map foo-mode-map
               (\"C-c C-c\" . (chimera \"chimera/foo\"
                                (when can-foo-p #'foo))))

This function enables this sort of usage; it is intended
as :filter-args advice for `bind-keys'."
  (let (elem output)
    (while args
      (setq elem (pop args))
      (cond
       ;; Skip keywords and their arguments
       ((keywordp elem) 
        (push elem output)
        (push (pop args) output)) 
       ;; Check cons cells for chimeras to expand
       ((consp elem) 
        (let ((def (cdr elem)))
          (when (chimera-p def :raw)
            (setf (cdr elem) (eval def)))
          (push elem output)))
       ;; Ignore anything we don't recognize
       (:else
        (push elem output))))
    (nreverse output)))

(advice-add 'bind-keys :filter-args #'chimera--bind-keys-advice)

(defun chimera--get-binding-description-advice (elem)
  "Advice to let `get-binding-description' handle menu items.

If the `bind-key' macro is passed a menu item as a command, a
subsequent call to `describe-personal-keybindings' will signal an
error because the function `get-binding-description' doesn't know
how to handle a menu item.  This function fixes that; it is
intended as :before-until advice on `get-binding-description'.

The description returned is the item name associated with the
menu item; e.g., for the menu item

  (menu-item \"foo\" nil
    :filter (can-foo-p))

the return value is \"foo\"."
  (when (chimera-p elem) (nth 1 elem)))

(advice-add 'get-binding-description :before-until
            #'chimera--get-binding-description-advice)


;;; Unloading
;;;==========
(defun chimera-unload-function ()
  "Undo changes made to Emacs for `chimera'.

More specifically, this function removes advice that `chimera'
applies to the macro `bind-keys' and the function
`get-binding-description' to add support for the `chimera' macro.

Because removing this advice will cause `get-binding-description'
to fail for menu items, we also need to remove all menu item
bindings from `personal-keybindings'.  But just doing this would
mean that `personal-keybindings' would no longer accurately
reflect the keybindings in use, so we also remove the keybindings
themselves (by binding them to nil).

Since the `chimera' macro is just a thin wrapper around the
built-in notion of a menu item, existing bindings made with
`chimera' don't need to be removed, unless they were made with
`bind-key' and thus appear in `personal-keybindings', as noted
above."
  (advice-remove #'bind-keys #'chimera--bind-keys-advice)
  (advice-remove #'get-binding-description
                 #'chimera--get-binding-description-advice)
  (when (boundp personal-keybindings)
    (setq personal-keybindings
          (cl-loop for ((key . map) cmd orig-cmd) in personal-keybindings
                   if (chimera-p cmd)
                   do (define-key
                        (symbol-value (or map 'global-map))
                        (kbd key)
                        nil)
                   else collect `((,key . ,map) ,cmd ,orig-cmd)))))

(provide 'chimera)
;;; chimera.el ends here
