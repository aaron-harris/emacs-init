;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MODE TAGS
;;;;============================================================================

;; This file contains code for defining and using "mode tags", which
;; support the collection of otherwise-unrelated modes under a common
;; umbrella and provide those collections with a common hook variable.

(require 'aph-symbol)
(require 'dash)                         ; For `-when-let'

(defmacro aph/def-mode-tag (tag &optional docstring)
  "Define TAG as a mode tag.

A mode tag is a way to designate several otherwise-unrelated
modes (major or minor) as sharing some particular
characteristic.

To tag a mode with a mode tag, use `aph/mode-tag-add'.  To remove
a tag, use `aph/mode-tag-remove'.  To check for the existence of
a tag, use `aph/mode-tag-p'.  To check for the association
between modes and tags, use `aph/mode-tag-p',
`aph/mode-tag-get-tags-for-mode', or
`aph/mode-tag-get-modes-for-tag'.

Instead of this macro, you can also use the function
`aph/mode-tag-create' to define mode tags.  As a function, it has
an interface more consistent with the other functions mentioned
above.

Defining a mode tag creates a hook variable named `TAG-tag-hook',
and all modes tagged with a tag (or derived from such a mode) run
this hook along with their individual mode hooks.  If a variable
with this name already exists (unless TAG is already a mode tag;
see below), an error is signalled and the tag is not created.

If a mode tag named TAG already exists, then its docstring is
updated to DOCSTRING and no other change is made. 

Information about mode tags is primarily stored using symbol
properties.  Four properties are used; except for `aph/mode-tag-tags',
these properties are associated with the symbol naming the mode tag.
- `aph/mode-tag' is set to t for symbols which name mode tags.
- `aph/mode-tag-modes' stores a list of all modes tagged with a
  particular tag.
- `aph/mode-tag-tags' (on the *mode* name) stores a list of all tags
  associated with that mode.
- `aph/mode-tag-docstring' stores the docstring for the tag.  The
  functionality to reference this (e.g., in a Help buffer) has not yet
  been implemented."
  (declare (debug (&define name [&optional stringp]))) 
  (let ((hook (aph/symbol-concat tag "-tag-hook")))
    `(progn
       (if (and (boundp ',hook) (not (aph/mode-tag-p ',tag)))
           (error "Variable %s already exists; tag %s not created"
                  ',hook ',tag)
         (put ',tag 'aph/mode-tag t)
         (put ',tag 'aph/mode-tag-docstring ,docstring)
         (defvar ,hook nil
           ,(format "Hook run for all modes tagged with the mode tag %s.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
                    tag))))))

(defun aph/mode-tag-create (tag &optional docstring)
  "Define TAG as a mode tag.

This is just a thin wrapper around `aph/def-mode-tag' with an
interface more consistent with other mode tag functions (e.g.,
`aph/mode-tag-delete').

More specifically, the problems this alleviates are these:
- Because `aph/def-mode-tag' is a macro and not a function, you do not
  need to quote TAG, unlike the functions used to manipulate mode
  tags, and this may be confusing.
- In order to use `aph/def-mode-tag' to create a mode tag whose name
  is obtained from a variable or from a function call (e.g., to use an
  uninterned symbol for testing purposes), you need to use `eval' and
  backquote.  This is unintuitive and ugly."
  (eval `(aph/def-mode-tag ,tag ,docstring)))

(defun aph/mode-tag-add (mode tag)
  "Tag MODE with TAG.
See `aph/def-mode-tag' for more information on mode tags."
  (cl-pushnew mode (get tag  'aph/mode-tag-modes))
  (cl-pushnew tag  (get mode 'aph/mode-tag-tags)))

(defun aph/mode-tag-remove (mode tag &optional nowarn)
  "If MODE is tagged with TAG, remove it.
If MODE is not tagged with TAG, print a warning message unless
the optional argument NOWARN is non-nil.

See `aph/def-mode-tag' for more information on mode tags."
  (aph/symbol-prop-delq mode tag  'aph/mode-tag-modes)
  (aph/symbol-prop-delq tag  mode 'aph/mode-tag-tags))

(defun aph/mode-tag-p (sym)
  "Return non-nil if SYM is the name of a mode tag.
See `aph/def-mode-tag' for more information on mode tags."
  (get sym 'aph/mode-tag))

(defun aph/mode-tag-tagged-p (mode tag &optional inherit)
  "Return non-nil if MODE is tagged with TAG.
If the optional parameter INHERIT is non-nil, also return non-nil
if any ancestor of MODE is tagged with TAG.

See `aph/def-mode-tag' for more information on mode tags."
  (or (cl-find mode (get tag 'aph/mode-tag-modes))
      (when inherit
        (-when-let (parent (get mode 'derived-mode-parent))
          (aph/mode-tag-tagged-p parent tag inherit)))))

(defun aph/mode-tag-get-tags-for-mode (mode)
  "Return a list of all mode tags on MODE.
See `aph/def-mode-tag' for more information on mode tags.")

(defun aph/mode-tag-get-modes-for-tag (tag)
  "Return a list of all modes tagged with TAG.
See `aph/def-mode-tag' for more information on mode tags.")

(provide 'aph-mode-tag)
