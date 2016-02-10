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
(ert-deftest aph/ert-test-with-major-mode--body ()
  "Test that body of `aph/ert-with-major-mode' is executed."
  (aph/ert-macro-executes-body aph/ert-with-major-mode mode 'text-mode))

(ert-deftest aph/ert-test-with-major-mode--bindings ()
  "Test that `aph/ert-with-major-mode' sets bindings correctly."
  ;; Intentional bindings
  (aph/ert-with-major-mode mode 'text-mode
    (should (fboundp mode))
    (should (eq mode-hook (aph/symbol-concat mode "-hook")))
    (should (boundp mode-hook))
    (should (eq mode-map (symbol-value (aph/symbol-concat mode "-map"))))
    (should (keymapp mode-map)))
  ;; No unintentional bindings
  (aph/ert-with-major-mode mode 'text-mode
    (should-error hook       :type 'void-variable)
    (should-error keymap-var :type 'void-variable)
    (should-error make-mode  :type 'void-variable)))

(ert-deftest aph/ert-test-with-major-mode--nesting ()
  "Test that `aph/ert-with-major-mode' nests properly."
  (aph/ert-with-major-mode mode1 'text-mode
    (aph/ert-with-major-mode mode2 mode1
      (should (eq 'text-mode (get mode1 'derived-mode-parent)))
      (should (eq mode1      (get mode2 'derived-mode-parent))))))

(ert-deftest aph/ert-test-with-major-mode--cleanup ()
  "Test that `aph/ert-with-major-mode' cleans up after itself."
  ;; Normal exit
  (let (mode-x hook-x keymap-x)
    (aph/ert-with-major-mode mode 'text-mode
      (setq mode-x    mode
            hook-x    mode-hook
            keymap-x  (aph/symbol-concat mode "-map")))
    (should-not (intern-soft mode-x))
    (should-not (intern-soft hook-x))
    (should-not (intern-soft keymap-x)))
  ;; Error
  (let (mode-x hook-x keymap-x)
    (ignore-errors
      (aph/ert-with-major-mode mode 'text-mode
        (setq mode-x    mode
              hook-x    mode-hook
              keymap-x  (aph/symbol-concat mode "-map"))
        (error "Triggered error")))
    (should-not (intern-soft mode-x))
    (should-not (intern-soft hook-x)))) 


(provide 'aph-ert-test)
