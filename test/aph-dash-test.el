;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DASH TESTS
;;;;============================================================================

;; Tests for the module `aph-dash'.
(require 'aph-dash)


;;; Equality Predicate Tests
;;;=========================
(ert-deftest aph/test-equality--all ()
  "Test `aph/equality' on `eq', `eql', and `equal'.
This test considers lists that pass or fail by all three
predicates together."
  (dolist (pred (list #'eq #'eql #'equal))
    (should     (aph/equality pred))
    (should     (aph/equality pred 'foo))
    (should     (aph/equality pred 'foo 'foo))
    (should     (aph/equality pred 1 1 1 1 1 1 1))
    (should-not (aph/equality pred 'foo 'bar)) 
    (should-not (aph/equality pred '(a b c) '(a b c) '(a b d)))
    (should-not (aph/equality pred [1 1 0] [1 1 0] [1 1 5]))
    (should-not (aph/equality pred "foo" "Foo" "foo"))
    (should-not (aph/equality pred 1 1.0 1))
    (should-not (aph/equality pred 'foo 'bar 'baz))
    (should-not (aph/equality pred 'foo "foo"))
    (should-not (aph/equality pred '(1 1 0) [1 1 0] [1 1 0]))))

(ert-deftest aph/test-equality--eql-not-eq ()
  "Test that `aph/equality' distinguishes `eq' from `eql'."
  (should-not (aph/eq              1.0 1.00 1.00000))
  (should     (aph/equality #'eql  1.0 1.00 1.00000))
  (should     (aph/equal           1.0 1.00 1.00000)))

(ert-deftest aph/test-equality--equal-not-eql ()
  "Test that `aph/equality' distinguishes `eql' from `equal'."
  (let ((test-lists '(('(a b c) '(a b c) '(a b c))
                      ([1 1 0] [1 1 0] [1 1 0])
                      ("foo" "foo" "foo") 
                      ('foo 'foo 'foo 'foo 'foo))))
    (dolist (data test-lists)
      (should (apply #'aph/equal data))
      (dolist (pred (list #'eq #'eql))
        (should-not (apply #'aph/equality pred data))))))

(provide 'aph-dash-test)
