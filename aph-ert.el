;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; ERT EXTENSIONS
;;;;============================================================================

;; Functions implementing general-purpose testing apparatus for use
;; with `ert'.
(require 'ert)


;;; Macro Testing Apparatus
;;;========================
(defmacro aph/ert-macro-executes-body (macro &rest other-args)
  "Execute a standard test to check that MACRO executes a body.

Apply MACRO to a body form, prepending any OTHER-ARGS, and check
both that this body was executed, and that the value returned is
the return value of its last form.  Return t if both conditions
hold, and signal an error with `should' otherwise."
  (let ((canary  (make-symbol "canary")))
    `(let (,canary)
       (should (= 7 (,macro ,@other-args
                            (setq ,canary t)
                            (+ 1 6))))
       (should ,canary))))


;;; Mode Testing Apparatus
;;;=======================
(defmacro aph/ert-with-test-mode (names parent &rest body)
  "Execute BODY in an environment with a temporarily-defined major mode.

More specifically:
- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and create a major mode (using `define-derived-mode') with this
  name.  Evaluate PARENT to get the name of the parent mode.
- Execute BODY, with the name of the new mode bound to MODE.  If HOOK
  is supplied, bind it to the name of the hook variable associated
  with the mode.  If KEYMAP is supplied, bind it to the name of the
  keymap variable associated with the mode.
- Make sure the mode created does not persist outside this form, using
  `unwind-protect' to ensure the tag is deleted in the event of an
  error or nonlocal exit from BODY.

Note that the major mode constructed in this block doesn't actually do
anything (i.e., its body is empty).

\(fn (MODE [HOOK [KEYMAP]]) PARENT &rest BODY)"
  (declare (debug ((symbolp &optional symbolp symbolp) form body))
           (indent 2))
  (let ((mode       (car names))
        (hook       (or (nth 1 names) (make-symbol "hook")))
        (keymap     (or (nth 2 names) (make-symbol "keymap")))
        (make-mode  (make-symbol "make-mode")))
    `(let* ((,mode       (cl-gensym "mode"))
            (,hook       (aph/symbol-concat ,mode "-hook"))
            (,keymap     (aph/symbol-concat ,mode "-map"))
            (,make-mode  (lambda (child parent)
                           (eval `(define-derived-mode
                                    ,child ,parent "Lighter")))))
       (unwind-protect
           (progn (funcall ,make-mode ,mode ,parent)
                  ,@body)
         (unintern ,hook)
         (unintern ,keymap)
         (unintern (aph/symbol-concat ,mode "-syntax-table"))
         (unintern (aph/symbol-concat ,mode "-abbrev-table"))))))


(provide 'aph-ert)
