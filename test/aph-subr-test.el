;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SUBROUTINE TESTS
;;;;============================================================================

;; Tests for the module aph-subr.el.
(require 'aph-subr)


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
