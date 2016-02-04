;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SIMPLE TESTS
;;;;============================================================================

;; Tests for the module `aph-simple'.
(require 'aph-simple)


;;; Yank Commands
;;;==============
(ert-deftest aph/test-yank-command-on-string ()
  "Test `aph/yank-command-on-string'." 
  (let (kill-ring)
    (with-temp-buffer
      (should (equal "foo"
                     (aph/yank-command-on-string "foo" #'yank)))
      (should (equal "" (buffer-string)))
      (should (equal "foo"
                     (aph/yank-command-on-string "bar" #'yank 2)))
      (should (equal "" (buffer-string)))
      (should (equal "baz"
                     (aph/yank-command-on-string "bar" #'insert "baz"))))))


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
