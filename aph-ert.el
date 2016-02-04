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


(provide 'aph-ert)
