;;; mode-family.el --- Hook any collection of modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions

;; Required features: `dash'

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

;; It is often convenient to run a function in the mode hook for
;; several distinct modes that may be conceptually similar in some way
;; but do not share ancestry (e.g., a programming language mode and
;; the mode for the associated REPL) or whose common ancestor also
;; includes other modes for which the function is not appropriate
;; (e.g., modes for all different kinds of lisps, whose common
;; ancestor is probably `prog-mode').
;;
;; The naive solution to this problem is just to add a given function
;; to multiple mode hooks, e.g.
;;
;;   (add-hook 'foo-mode-hook #'frobnicate)
;;   (add-hook 'bar-mode-hook #'frobnicate)
;;
;; There are two problems with this approach:
;;
;; 1. It's untidy, especially if there are more than two or three
;;    modes to be considered.
;;
;; 2. If there are several functions that are all being added to the
;;    same list of modes, then adding or removing a mode from this set
;;    must be done in many different places.
;;
;; 3. If you're trying to keep configuration for different packages
;;    separate (e.g., with `use-package'), then do you keep this with
;;    the package defining the hook function (frobnicate, in the above
;;    example), or split it up so it's with the packages defining the
;;    individual modes?
;;
;; This module attempts to solve these problems by introducing the
;; concept of a "mode family".  Each mode family has its own hook,
;; which is run by all the members of the family with their usual mode
;; hooks.
;;
;; To create a mode family, use the function `mode-family-create'.
;; (This is autoloaded, so there is no need to require `mode-family'.)
;; Once you have a family, you can populate it with `mode-family-add'
;; and remove members with `mode-family-remove'.
;;
;; The hook associated with the mode family foo is named
;; foo-family-hook.  You can add functions to this hook with
;; `add-hook', as usual.
;;
;; The remaining functions in this module allow for inspection of mode
;; families.  To test for the existence of a mode family, see
;; `mode-family-p'.  To test for membership in a family, see
;; `mode-family-member-p'.  To get a list of all members belonging to
;; a particular family, see `mode-family-list-members', and to get a
;; list of all families to which a particular mode belongs, see
;; `mode-family-list-families'.

;;; Code:

(require 'dash)       ; For `-when-let', `-union', `-reduce-from', `-lambda'

(eval-when-compile (require 'aph-subr))   ; For `aph/set-assq'



;;; Internal Variables
;;;===================
(defvar mode-family--list nil
  "The list of all mode families.

Do not modify this list directly; use `mode-family-create'
instead.")

(defvar mode-family--alist nil
  "An alist associating modes to families.

The car of each element should be a symbol naming a mode; the
associated cdr is a list of all mode families to which that mode
belongs.

Do not modify this list directly; use `mode-family-add' and
`mode-family-remove' instead.")


;;; Mode Family Creation
;;;=====================
(defmacro define-mode-family-hook (name family)
  "Define NAME as a hook variable for FAMILY.
Do not use this directly; use `mode-family-create' instead."
  (declare (debug (&define name symbolp)))
  `(defvar ,name nil
     ,(format "Hook run for all modes belonging to the mode family %s.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
              family)))

(defun mode-family--hook (family)
  "Return the name of the hook variable for FAMILY (a symbol).

This function is syntactic only; it is not necessary that FAMILY
actually be a mode family."
  (intern (concat (symbol-name family) "-family-hook")))

;;;###autoload
(defun mode-family-create (family)
  "Define FAMILY as a mode family.
If one already exists, do nothing.

A mode family is a way to designate several otherwise-unrelated
modes (major or minor) as sharing some particular characteristic.
Mode families are named using symbols in the standard obarray,
but this does not use either a symbol's variable slot or its
function slot, so the same symbol can name both a mode family and
a variable and/or function.

To associate a mode with a mode family, see `mode-family-add';
this creates the family if it doesn't already exist.  To remove a
mode from a family, see `mode-family-remove'.  To check for the
existence of a family, see `mode-family-exists-p'.  To check
whether a mode is part of a family, see `mode-family-member-p',
`mode-family-list-members', or `mode-family-list-families'.

Each mode family has a hook, named `FAMILY-family-hook', and all
modes associated with a family (or derived from such a mode) run
this hook along with their individual mode hooks.

In the event that a variable already exists with the same name as
the hook variable for the new family, it will be \"stolen\" by
the mode family (without warning!), but its value will not be
changed; this is necessary in order to allow for out-of-order
hooks."
  (declare (indent defun))
  (unless (mode-family-p family)
    (push family mode-family--list) 
    (eval `(define-mode-family-hook ,(mode-family--hook family) ,family))))


;;; Predicates
;;;===========
(defun mode-family-p (sym)
  "Return non-nil if SYM is the name of a mode family.

See the documentation for `mode-family-create' for more information."
  (memq sym mode-family--list))

(defun mode-family-member-p (mode family &optional inherit)
  "Return non-nil if MODE is a member of FAMILY.

If the optional parameter INHERIT is non-nil, also return non-nil
if any ancestor of MODE is a member of FAMILY.

See the documentation for `mode-family-create' for more information."
  (let ((mode-families  (assoc-default mode mode-family--alist #'eq)))
    (or (memq family mode-families)
        (when inherit
          (-when-let (parent (get mode 'derived-mode-parent))
            (mode-family-member-p parent family inherit))))))


;;; Membership Lists
;;;=================
(defun mode-family-list-families (mode &optional inherit)
  "Return a list of all families to which MODE belongs.

If the optional parameter INHERIT is non-nil, also include all
families to which an ancestor of MODE belongs.  If multiple
ancestors belong to the same family, it is included only once.

See the documentation for `mode-family-create' for more information."
  (-union (assoc-default mode mode-family--alist #'eq)
          (-when-let (parent (and inherit (get mode 'derived-mode-parent)))
            (mode-family-list-families
             (get mode 'derived-mode-parent) inherit))))

(defun mode-family-list-members (family)
  "Return a list of all modes belonging to FAMILY.

See the documentation for `mode-family-create' for more information."
  (-reduce-from (-lambda (acc (mode . families))
                  (if (memq family families)
                      (cons mode acc)
                    acc))
                nil
                mode-family--alist))


;;; Membership Management
;;;======================
;;;###autoload
(defun mode-family-add (mode family)
  "Add MODE to FAMILY.  If FAMILY doesn't exist, create it.
If MODE is already a member of FAMILY, do nothing.

See the documentation for `mode-family-create' for more information." 
  (unless (mode-family-p family) (mode-family-create family))
  (unless (mode-family-member-p mode family)
    (let ((families (assoc-default mode mode-family--alist)))
      (aph/set-assq mode-family--alist mode (push family families)))))

(defun mode-family-remove (mode family)
  "If MODE is a member of FAMILY, remove it.
Otherwise, do nothing.

See the documentation for `mode-family-create' for more information."
  (aph/set-assq mode-family--alist
                mode
                (remove family (mode-family-list-families mode))))


;;; Hook Functionality
;;;===================
(defun mode-family-run-hooks ()
  "Run hooks for all mode families associated with current major mode.

If the major mode is a derived mode, also run the hooks for mode
families associated with all its ancestors.

This function is automatically run whenever the major mode is changed.

See the documentation for `mode-family-create' for more information."
  (apply #'run-hooks
         (mapcar #'mode-family--hook
                 (mode-family-list-families major-mode :inherit))))

(add-hook 'change-major-mode-after-body-hook #'mode-family-run-hooks)


;;; Unloading
;;;==========
(defun mode-family-unload-function ()
  "Undo changes made to Emacs by mode-family.el.

More specifically:

- Remove `mode-family-run-hooks' from
  `change-major-mode-after-body-hook'.

- Delete all mode family hooks."
  (remove-hook 'change-major-mode-after-body-hook #'mode-family-run-hooks)
  (dolist (family mode-family--list)
    (makunbound (mode-family--hook family))))


(provide 'mode-family)
;;; mode-family.el ends here
