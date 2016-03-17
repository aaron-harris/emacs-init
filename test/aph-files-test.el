;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; FILES TESTS
;;;;============================================================================

;; Tests for the module `aph-files'.
(require 'aph-files)


;;; Killing Buffers
;;;================
(ert-deftest aph/files-test-kill-buffer-if-any ()
  "Test `aph/kill-buffer-if-any'."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (aph/kill-buffer-if-any buf :nowarn)
      (should-not (buffer-live-p buf))
      (aph/kill-buffer-if-any buf :nowarn)))
  (with-temp-buffer
    (let ((name (buffer-name)))
      (aph/kill-buffer-if-any name :nowarn)
      (should-not (buffer-live-p (get-buffer name)))
      (aph/kill-buffer-if-any name :nowarn))))


(provide 'aph-files-test)
