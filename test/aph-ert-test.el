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


(provide 'aph-ert-test)
