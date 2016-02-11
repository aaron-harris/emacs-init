;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ERT TESTS
;;;;============================================================================

;; Tests for the module `aph-ert'.
(require 'aph-ert)


;;; Macro Testing Apparatus Tests
;;;==============================
(ert-deftest aph/ert-test-macro-executes-body-p ()
  "Test `aph/ert-macro-executes-body-p'." 
  (should (aph/ert-macro-executes-body-p 'with-temp-buffer))
  (should (aph/ert-macro-executes-body-p 'let '((canary))))
  (should-error (aph/ert-macro-executes-body-p 'ignore)))


;;; Mode Testing Apparatus Tests: Parametrizations
;;;===============================================
(defun aph/ert-test-mode-wrapper--bindings (macro &optional other-args)
  "Test that MACRO binds variables per `aph/ert--with-test-mode'.

Here MACRO should be a symbol naming a macro with the same
general contract as `aph/ert--with-test-mode'.  That is:
- MACRO should have a signature like
  (MACRO NAME [OTHER-ARGS ...] &rest BODY).
- Inside BODY, the variables NAME, NAME-hook, and NAME-map should be
  respectively bound to a mode of some kind, that mode's hook
  variable, and the mode's keymap.
- The above bindings should not persist outside of BODY.

Run a test confirming that the bindings are all appropriately
made inside BODY.  If they are, return t.  Otherwise, signal an
error using `should'."
  (let ((test
         `(,macro mode ,@other-args
                  (should (fboundp mode))
                  (should (eq mode-hook
                              (aph/symbol-concat mode "-hook")))
                  (should (boundp mode-hook))
                  (should (eq mode-map
                              (symbol-value (aph/symbol-concat mode "-map"))))
                  (should (keymapp mode-map)))))
    (eval test)))

(defun aph/ert-test-mode-wrapper--cleanup (macro &optional other-args)
  "Test that MACRO cleans up after itself.

Here MACRO should be a symbol naming a macro with the same
general contract as `aph/ert--with-test-mode'.  That is: - MACRO
should have a signature like
  (MACRO NAME [OTHER-ARGS ...] &rest BODY).
- Inside BODY, the variables NAME, NAME-hook, and NAME-map should be
  respectively bound to a mode of some kind, that mode's hook
  variable, and the mode's keymap.
- The above bindings should not persist outside of BODY.

Run a test confirming that the bindings do not persist after BODY
exits.  If they do not, return t.  Otherwise, signal an error
using `should'."
  (let ((subtest
         (lambda (wrap form)
           `(let (mode-x hook-x keymap-x)
                    (,wrap
                     (,macro mode ,@other-args
                             (setq mode-x    mode)
                             (setq hook-x    mode-hook)
                             (setq keymap-x  (aph/symbol-concat mode "-map"))
                             ,form)
                     (should-not (intern-soft mode-x))
                     (should-not (intern-soft hook-x))
                     (should-not (intern-soft keymap-x)))))))
    (eval (funcall subtest 'progn         '(ignore)))
    (eval (funcall subtest 'ignore-errors '(error "Triggered error")))
    t))


;;; Mode Testing Apparatus Tests: `aph/ert-with-major-mode'
;;;========================================================
(ert-deftest aph/ert-test-with-major-mode--body ()
  "Test that body of `aph/ert-with-major-mode' is executed."
  (should (aph/ert-macro-executes-body-p
           'aph/ert-with-major-mode '(mode 'fundamental-mode))))

(ert-deftest aph/ert-test-with-major-mode--bindings ()
  "Test that `aph/ert-with-major-mode' sets bindings correctly."
  (should (aph/ert-test-mode-wrapper--bindings
           'aph/ert-with-major-mode '('fundamental-mode))))

(ert-deftest aph/ert-test-with-major-mode--cleanup ()
  "Test that `aph/ert-with-major-mode' cleans up after itself."
  (should (aph/ert-test-mode-wrapper--cleanup
           'aph/ert-with-major-mode '('fundamental-mode))))

(ert-deftest aph/ert-test-with-major-mode--nesting ()
  "Test that `aph/ert-with-major-mode' nests properly."
  (aph/ert-with-major-mode mode1 'text-mode
    (aph/ert-with-major-mode mode2 mode1
      (should (eq 'text-mode (get mode1 'derived-mode-parent)))
      (should (eq mode1      (get mode2 'derived-mode-parent))))))


(provide 'aph-ert-test)
