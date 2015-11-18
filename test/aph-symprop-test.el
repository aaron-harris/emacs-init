;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SYMBOL PROPERTY TESTS
;;;;============================================================================

;; Tests for the module aph-symprop.el.

(require 'cl-lib)                       ; For `cl-gensym'

(ert-deftest aph/symprop-test-delq ()
  "Test functionality of `aph/symprop-delq'."
  (let ((sym  (cl-gensym "sym")))
    ;; Delete from middle
    (put sym 'prop '(foo bar baz))
    (aph/symprop-delq 'bar sym 'prop)
    (should (equal (get sym 'prop) '(foo baz)))
    ;; Delete from front
    (aph/symprop-delq 'foo sym 'prop)
    (should (equal (get sym 'prop) '(baz))) 
    ;; Element not present
    (aph/symprop-delq 'foo sym 'prop)
    (should (equal (get sym 'prop) '(baz)))
    ;; Delete last element
    (aph/symprop-delq 'baz sym 'prop)
    (should (equal (get sym 'prop) nil))
    ;; List is empty
    (aph/symprop-delq 'foo sym 'prop)
    (should (equal (get sym 'prop) nil)))) 
      
(provide 'aph-symprop-test)
