;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ERT TESTS
;;;;============================================================================

;; Tests for the module `aph-ert'.
(require 'aph-ert)


;;; Macro Testing Apparatus Tests
;;;==============================
(ert-deftest aph/ert-test-macro-executes-body ()
  "Test `aph/ert-macro-executes-body'." 
  (should (aph/ert-macro-executes-body with-temp-buffer))
  (should (aph/ert-macro-executes-body let (canary)))
  (should-error (aph/ert-macro-executes-body ignore)))


;;; Mode Testing Apparatus Tests
;;;=============================
(ert-deftest aph/ert-test-with-test-mode--body ()
  "Test that body of `aph/ert-with-test-mode' is executed."
  (aph/ert-macro-executes-body aph/ert-with-test-mode (mode) 'text-mode))

(ert-deftest aph/ert-test-with-test-mode--bindings ()
  "Test that `aph/ert-with-test-mode' sets bindings correctly."
  ;; Intentional bindings
  (aph/ert-with-test-mode (mode hook keymap) 'text-mode
    (should (fboundp mode))
    (should (eq hook (aph/symbol-concat mode "-hook")))
    (should (boundp hook))
    (should (eq keymap (aph/symbol-concat mode "-map")))
    (should (boundp keymap)))
  ;; No unintentional bindings
  (aph/ert-with-test-mode (mode) 'text-mode
    (should-error hook      :type 'void-variable)
    (should-error make-mode :type 'void-variable)))

(ert-deftest aph/ert-test-with-test-mode--nesting ()
  "Test that `aph/ert-with-test-mode' nests properly."
  (aph/ert-with-test-mode (mode1) 'text-mode
    (aph/ert-with-test-mode (mode2) mode1
      (should (eq 'text-mode (get mode1 'derived-mode-parent)))
      (should (eq mode1      (get mode2 'derived-mode-parent))))))

(ert-deftest aph/ert-test-with-test-mode--cleanup ()
  "Test that `aph/ert-with-test-mode' cleans up after itself."
  ;; Normal exit
  (let (mode-x hook-x keymap-x)
    (aph/ert-with-test-mode (mode hook keymap) 'text-mode
      (setq mode-x   mode
            hook-x   hook
            keymap-x keymap))
    (should-not (intern-soft mode-x))
    (should-not (intern-soft hook-x))
    (should-not (intern-soft keymap-x)))
  ;; Error
  (let (mode-x hook-x keymap-x)
    (ignore-errors
      (aph/ert-with-test-mode (mode hook keymap) 'text-mode
        (setq mode-x   mode
              hook-x   hook
              keymap-x keymap)
        (error "Triggered error")))
    (should-not (intern-soft mode-x))
    (should-not (intern-soft hook-x)))) 


(provide 'aph-ert-test)
