;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SYMBOL PROPERTY TESTS
;;;;============================================================================

;; Tests for the module aph-symbol.el.

(require 'cl-lib)                       ; For `cl-gensym'

(ert-deftest aph/symbol-test-concat ()
  "Test functionality of `aph/symbol-test-concat'."
  (should (equal "foo-bar" (symbol-name (aph/symbol-concat 'foo "-bar"))))
  (should (equal "foo-bar"
                 (symbol-name (aph/symbol-concat (make-symbol "foo") "-bar"))))
  (should (intern-soft (aph/symbol-concat (make-symbol "foo") "-bar"))))

(ert-deftest aph/symbol-test-prop-delq ()
  "Test functionality of `aph/symbol-prop-delq'."
  (let ((sym  (cl-gensym "sym")))
    ;; Delete from middle
    (put sym 'prop '(foo bar baz))
    (aph/symbol-prop-delq 'bar sym 'prop)
    (should (equal (get sym 'prop) '(foo baz)))
    ;; Delete from front
    (aph/symbol-prop-delq 'foo sym 'prop)
    (should (equal (get sym 'prop) '(baz))) 
    ;; Element not present
    (aph/symbol-prop-delq 'foo sym 'prop)
    (should (equal (get sym 'prop) '(baz)))
    ;; Delete last element
    (aph/symbol-prop-delq 'baz sym 'prop)
    (should (equal (get sym 'prop) nil))
    ;; List is empty
    (aph/symbol-prop-delq 'foo sym 'prop)
    (should (equal (get sym 'prop) nil)))) 
      
(provide 'aph-symbol-test)
