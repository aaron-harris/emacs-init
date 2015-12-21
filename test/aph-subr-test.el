;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SUBROUTINE TESTS
;;;;============================================================================

;; Tests for the module aph-subr.el.
(require 'aph-subr)

(ert-deftest aph/test-get-bol/-eol ()
  "Test functions `aph/get-bol' and `aph/get-eol'."
  (with-temp-buffer
    (should (= (aph/get-bol) (aph/get-bol (point))
               (aph/get-eol) (aph/get-eol (point))
               (point) (point-min) (point-max)))
    (insert "A")
    (should (= (aph/get-bol) (aph/get-bol (point)) 1))
    (should (= (aph/get-eol) (aph/get-eol (point)) 2 (point)))
    (insert "B") 
    (should (= (aph/get-bol) (aph/get-bol (point)) 1))
    (should (= (aph/get-eol) (aph/get-eol (point)) 3 (point)))
    (insert "\n")
    (should (= (aph/get-bol 1) (aph/get-bol 3) 1))
    (should (= (aph/get-eol 1) (aph/get-eol 3) 3)) 
    (should (= (aph/get-bol) (aph/get-bol (point))
               (aph/get-eol) (aph/get-eol (point)) 4 (point)))
    (insert "foo\n\nbar")
    (should (= (aph/get-bol 4) (aph/get-bol 6) 4))
    (should (= (aph/get-eol 4) (aph/get-eol 6) 7))
    (should (= (aph/get-bol 8) (aph/get-eol 8) 8))
    (should (= (aph/get-bol 9) (aph/get-bol) 9))
    (should (= (aph/get-eol 9) (aph/get-eol) 12 (point)))))

      
(provide 'aph-subr-test)
