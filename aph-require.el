;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; REQUIRE FUNCTIONS
;;;;============================================================================

;;; This file contains functions extending the `require' functionality.


;;; Soft Require
;;;=============
(defun aph/require-softly (feature &optional filename)
  "As `require', but instead of an error just print a message.

If there is an error, its message will be included in the message
printed.

Like `require', the return value will be FEATURE if the load was
successful (or unnecessary) and nil if not."
  (condition-case err
      (require feature filename) 
    (error (message (concat "Error loading %s"
                            (if filename " (%s): \"" ": \"")
                            (error-message-string err)
                            "\"")
                    feature filename)
           nil)))


;;; Machine-Specific Require
;;;=========================
(defun aph/require-only-for-machine (machine feature &optional filename)
  "As `aph/require-softly', but only for MACHINE.

If `aph/machine' is MACHINE (a symbol), require FEATURE
using `aph/require-softly'.  Otherwise, do nothing."
  (when (eq machine aph/machine)
    (aph/require-softly feature filename)))

(defun aph/require-except-for-machine (machine feature &optional filename)
  "As `aph/require-softly', but not for MACHINE.

If `aph/machine' is not MACHINE (a symbol), require FEATURE
using `aph/require-softly'.  Otherwise, do nothing."
  (unless (eq machine aph/machine)
    (aph/require-softly feature filename)))

(provide 'aph-require)
