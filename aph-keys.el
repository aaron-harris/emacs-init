;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; KEYBINDING UTILITIES
;;;;============================================================================

;; Some utilities related to keybindings.

(require 'aph-bind-key)                 ; For `bind-keys' and fixes


;;; Conditional Keybindings
;;;========================
;; Code in this section aims to establish conditional keybindings and
;; is adapted from a macro found here:
;;   http://endlessparentheses.com/define-context-aware-keys-in-emacs.html

(defun aph/keys--construct-conditional-binding (def condition)
  "Return a menu item defining a binding for DEF subject to CONDITION.

The returned menu item can be used with `define-key' to define a
binding for COMMAND that is made transparent whenever
CONDITION (a single form) evaluates to nil."
  `(menu-item
    ,(format "maybe-%s" (or (car (cdr-safe def)) def))
    nil
    :filter (lambda (&optional _)
              (when ,condition ,def))))

(defmacro aph/keys-define-conditionally (keymap key def &rest body)
  "In KEYMAP, define key sequence KEY as DEF conditionally.
This is like `define-key', except the definition
\"disappears\" whenever BODY evaluates to nil."
  (declare (indent 3)
           (debug (form form form &rest sexp)))
  `(define-key ,keymap ,key
     ',(aph/keys--construct-conditional-binding def `(progn ,@body))))

(defmacro aph/bind-key-conditionally (key-name command keymap &rest body)
  "As `bind-key', but use `aph/keys-define-conditionally'."
  (declare (indent 3)
           (debug (stringp sexp keymapp body)))
  (let ((conditional-binding  (aph/keys--construct-conditional-binding
                               `',command `(progn ,@body))))
    `(bind-key ,key-name ',conditional-binding ,keymap)))

(defun aph/bind-keys-when-advice (args)
  "Advice to add support for :when keyword to `bind-keys'.

The :when keyword adds a condition to all bindings following it.
A subsequent :when condition will replace any previous
condition.

Note that, unlike `aph/bind-key-conditionally', only a single
form can follow a :when keyword.

For example, the form

  (bind-keys :map foo-mode-map
             (\"a\" . foo)
             :when (bar-p)
             (\"b\" . bar)
             :when (baz-p)
             (\"c\" . baz))

is equivalent to this code:

  (bind-key \"a\" foo foo-mode-map)
  (aph/bind-key-conditionally \"b\" bar foo-mode-map (bar-p))
  (aph/bind-key-conditionally \"c\" baz foo-mode-map (baz-p))

This is intended as :filter-args advice for `bind-keys'."
  (let (elem output condition)
    (while args
      (setq elem (pop args))
      (cond
       ((eq elem :when)
        (setq condition (pop args)))

       ((keywordp elem)
        (push elem output)
        (push (pop args) output))

       ((null condition)
        (push elem output))

       (:else
        (let ((key  (car elem))
              (def  (cdr elem)))
          (push `(,key . ,(aph/keys--construct-conditional-binding
                           `',def condition))
                output)))))
    (nreverse output)))

(advice-add 'bind-keys :filter-args #'aph/bind-keys-when-advice)


(provide 'aph-keys)
