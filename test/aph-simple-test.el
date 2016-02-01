;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SIMPLE TESTS
;;;;============================================================================

;; Tests for the module `aph-simple'.
(require 'aph-simple)


;;; Motion Commands
;;;================
(ert-deftest aph/simple-test-move-bol ()
  "Test `aph/move-beginning-of-line'."
  (with-temp-buffer
    (insert "foo")                                        ; "foo|"
    (should (= 1 (aph/move-beginning-of-line) (point)))   ; "|foo"
    (insert "  ")                                         ; "  |foo"
    (should (= 1 (aph/move-beginning-of-line) (point)))   ; "|  foo"
    (should (= 3 (aph/move-beginning-of-line) (point)))   ; "  |foo"
    (open-line 1)                                         ; "  |"/"foo"
    (should (= 4 (aph/move-beginning-of-line 2) (point))) ; "  "/"|foo"
    ))

      
(provide 'aph-simple-test)
