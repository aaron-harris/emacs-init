;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SUBROUTINE TESTS
;;;;============================================================================

;; Tests for the module aph-subr.el.
(require 'aph-subr)


;;; Buffer Position Functions
;;;==========================
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


;;; Alist Functions
;;;================
(defun aph/subr-test-update-alist (&optional fun)
  "Subroutine for testing `aph/update-alist'.
Run a standard test to verify basic functionality of
`aph/update-alist' when called with FUN as optional fourth
argument.  Return t if the test succeeds, and signal an error
otherwise, using `should'."
  (require 'dash)                       ; For `-zip'
  ;; The only reason we're using `-zip' here instead of writing
  ;;   '((foo . 1) (bar . 2) (foo . 3))
  ;; is that the latter is a literal list, and modifying a literal
  ;; list does not act as expected.
  (let ((alist (-zip '(foo bar foo) '(1 2 3))))
    (should (equal (setq alist (aph/update-alist alist 'foo 4 fun))
                   '((foo . 4) (bar . 2) (foo . 3))))
    (should (equal (setq alist (aph/update-alist alist 'bar 5 fun))
                   '((foo . 4) (bar . 5) (foo . 3))))
    (should (equal (setq alist (aph/update-alist alist 'baz 6 fun))
                   '((baz . 6) (foo . 4) (bar . 5) (foo . 3))))
    t))

(ert-deftest aph/test-update-alist ()
  "Test `aph/update-alist'."
  (should (aph/subr-test-update-alist))
  (should (aph/subr-test-update-alist #'assoc))
  (should (aph/subr-test-update-alist #'assq)))

      
(provide 'aph-subr-test)
