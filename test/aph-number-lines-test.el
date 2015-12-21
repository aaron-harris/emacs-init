;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; LINE NUMBERING TESTS
;;;;============================================================================

;; Tests for the module aph-number-lines.el
(require 'aph-number-lines)

(ert-deftest aph/test-number-lines ()
  "Test basic functionality of `aph/number-lines'."
  (with-temp-buffer
    (aph/number-lines (point-min) (point-max))
    (should (equal (buffer-string) "1 ")))
  (with-temp-buffer
    (insert "A\nB\nC")
    (aph/number-lines (point-min) (point-max))
    (should (equal (buffer-string) "1 A\n2 B\n3 C")))
  (with-temp-buffer
    (insert (make-string 99 ?\n))
    (aph/number-lines (point-min) (point-max))
    (should (equal (buffer-string) 
                   (mapconcat (apply-partially #'format "%3d ")
                              (number-sequence 1 100)
                              "\n")))))

      
(provide 'aph-number-lines-test)
