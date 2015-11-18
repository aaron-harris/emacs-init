;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MODE TAGS
;;;;============================================================================

;; This file contains code for defining and using "mode tags", which
;; support the collection of otherwise-unrelated modes under a common
;; umbrella and provide those collections with a common hook variable.

(defun aph/mode-tag-hook-var (symbol)
  "Return the name of the hook variable for a mode tag named SYMBOL.
Note that SYMBOL need not be defined as a mode tag, and hence the
value returned is not necessarily bound as a variable."
  (intern (concat (symbol-name symbol) "-tag-hook")))

(defmacro aph/mode-tag-create (tag &optional docstring)
  "Define TAG as a mode tag.

A mode tag is a way to designate several otherwise-unrelated
modes (major or minor) as sharing some particular
characteristic.

To tag a mode with a mode tag, use `aph/mode-tag-add'.  To remove
a tag, use `aph/mode-tag-remove'.  To check for the existence of
a tag, use `aph/mode-tag-p'.  To check for the association
between modes and tags, use `aph/mode-tag-p',
`aph/mode-tag-get-tags-for-mode', or
`aph/mode-tag-get-modes-for-tag'.  To delete an existing mode
tag, use `aph/mode-tag-delete'.

Defining a mode tag creates a hook variable named `TAG-tag-hook',
and all modes tagged with a tag (or derived from such a mode) run
this hook along with their individual mode hooks.  If a variable
with this name already exists, an error is signalled and the tag
is not created.

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
  (let ((hook (aph/mode-tag-hook-var tag)))
    `(progn
       (if (boundp ',hook)
           (error "Variable %s already exists; tag %s not created"
                  ',hook ',tag)
         (put ',tag 'aph/mode-tag t)
         (put ',tag 'aph/mode-tag-docstring ,docstring)
         (defvar ,hook nil
           ,(format "Hook run for all modes tagged with the mode tag %s.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
                    tag))))))

(defun aph/mode-tag-delete (tag)
  "Delete TAG as a mode tag.
See `aph/mode-tag-create' for more information on mode tags."
  (let ((hook (aph/mode-tag-hook-var tag)))
    (put tag 'aph/mode-tag nil)
    (put tag 'aph/mode-tag-docstring nil)
    (makunbound hook)
    (put hook 'variable-documentation nil)))

(defun aph/mode-tag-add (mode tag)
  "Tag MODE with TAG.
See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-remove (mode tag &optional nowarn)
  "If MODE is tagged with TAG, remove it.
If MODE is not tagged with TAG, print a warning message unless
the optional argument NOWARN is non-nil.

See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-p (sym)
  "Return non-nil if SYM is the name of a mode tag.
See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-tagged-p (mode tag &optional inherit)
  "Return non-nil if MODE is tagged with TAG.
If the optional parameter INHERIT is non-nil, also return non-nil
if any ancestor of MODE is tagged with TAG.

See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-get-tags-for-mode (mode)
  "Return a list of all mode tags on MODE.
See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-get-modes-for-tag (tag)
  "Return a list of all modes tagged with TAG.
See `aph/mode-tag-create' for more information on mode tags.")

(provide 'aph-mode-tag)
