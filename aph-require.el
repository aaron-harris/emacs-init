;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; REQUIRE FUNCTIONS
;;;;============================================================================

;;; This file contains functions extending the `require' functionality.

;;; Font Lock Setup
;;;================
;; After each function, we add the font lock details for that function
;; to this list.  To implement them, call
;; `aph/require-enable-font-lock'.
(defvar aph/require--font-lock
  nil
  "For internal use by `aph/require-enable-font-lock'.")


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
;; Font lock: Highlight FEATURE, if a symbol, in `font-lock-constant-face'.
;;   This follows the font lock pattern for `require'.
(add-to-list
 'aph/require--font-lock
 '("\(aph/require-softly\\s-+'\\(\\_<.+?\\_>\\)" 1 font-lock-constant-face))


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


;;; Font Lock
;;;==========
(defun aph/require-enable-font-lock ()
  "Add syntax highlighting for functions from aph/require."
  (font-lock-add-keywords 'emacs-lisp-mode aph/require--font-lock))

(provide 'aph-require)
