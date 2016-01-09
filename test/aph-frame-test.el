;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; FRAME TESTS
;;;;============================================================================

;; Tests for the module `aph-frame'.
(require 'aph-frame)

(ert-deftest aph/frame-test-true-width ()
  "Test `aph/frame-true-width'."
  (aph/save-frame-excursion
    (let* ((frame (make-frame)))
      (set-frame-width frame 100 nil :pixelwise)
      (should (= 100 (aph/frame-true-width frame))))))

      
(provide 'aph-frame-test)
