;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING FUNCTIONS
;;;;============================================================================

;;; This file contains custom functions dealing with keybinding.


;;; Safe Keybinding
;;;================
;; Functions in this section make defining keys safer, handling
;; exceptional conditions more gracefully than the default functions.

(defun aph/define-key-safely (keymap key command
                                     &optional package nomsg-on-rebind)
  "As `define-key', but check if COMMAND is defined first.

If COMMAND is defined, proceed to bind it to KEY in KEYMAP.  If
it isn't, print a message to that effect and do not bind it.

If the first optional parameter PACKAGE is supplied, delay the
keybinding until after PACKAGE is loaded, using
`eval-after-load'.

Unless the second optional parameter NOMSG-ON-REBIND is supplied,
also print a message if we are overwriting an existing binding
for KEY.  Proceed with the rebinding in any case.

If the binding succeeds, return COMMAND.  Otherwise return nil."
  (if package
      (eval-after-load package
        '(aph/define-key-safely keymap key command nil nomsg-on-rebind))
    (catch 'esc
      (let ((old-binding (lookup-key keymap key)))
      (if (fboundp command)
          (define-key keymap key command)
        (message "Cannot bind #'%s to %s: Command not defined"
                 command (key-description key))
        (throw 'esc nil))
      (unless (or nomsg-on-rebind
                  (not old-binding)
                  (eq old-binding command))
        (message "Warning: Binding #'%s to %s overwrites existing binding #'%s"
                 command (key-description key) old-binding))
      command))))

(defmacro aph/define-keys-safely (keymap package &rest bindings)
  "Define multiple keys using `aph/define-key-safely'.

All BINDINGS must be for the same KEYMAP, and the PACKAGE
parameter applies to all keybindings.  Each binding should be of
the form (KEY COMMAND NOMSG-ON-REBIND).  See
`aph/define-key-safely' for details.

Unlike `aph/define-key-safely', the parameter PACKAGE is not
optional and must be explicitly set to nil if its effect is not
needed."
  (declare (indent 2))
  `(progn
     ,@(mapcar
        (lambda (keydef)
          (let ((key              (nth 0 keydef))
                (command          (nth 1 keydef))
                (nomsg-on-rebind  (nth 2 keydef)))
            `(aph/define-key-safely ,keymap ,key ,command
                                    ,package ,nomsg-on-rebind)))
        bindings)))

(defun aph/global-set-key-safely (key command
                                      &optional package nomsg-on-rebind)
  "As `aph/define-key-safely', but use the global keymap."
  (aph/define-key-safely (current-global-map)
                         key command package nomsg-on-rebind))

(defmacro aph/global-set-keys-safely (package &rest bindings)
  "As `aph/define-keys-safely', but use the global keymap."
  (declare (indent 1)) 
  `(aph/define-keys-safely (current-global-map) ,package ,@bindings))

(provide 'aph-keys)
