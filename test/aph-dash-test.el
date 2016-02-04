;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DASH TESTS
;;;;============================================================================

;; Tests for the module `aph-dash'.
(require 'aph-dash)


;;; Equality Predicate Tests
;;;=========================
(ert-deftest aph/test-equal ()
  "Test `aph/equal'."
  ;; Edge cases
  (should     (aph/equal))
  (should     (aph/equal 'foo))
  (should     (aph/equal 'foo 'foo))
  (should-not (aph/equal 'foo 'bar))
  ;; Content tests
  (should     (aph/equal '(a b c) '(a b c) '(a b c)))
  (should-not (aph/equal '(a b c) '(a b c) '(a b d)))
  (should     (aph/equal [1 1 0]  [1 1 0]  [1 1 0]))
  (should-not (aph/equal [1 1 0]  [1 1 0]  [1 1 5]))
  (should     (aph/equal "foo"    "foo"    "foo"))
  (should-not (aph/equal "foo"    "Foo"    "foo"))
  (should     (aph/equal 1        1        1))
  (should-not (aph/equal 1        1.0      1))
  (should     (aph/equal 'foo     'foo     'foo))
  (should-not (aph/equal 'foo     'bar     'baz))
  ;; Mixed content
  (should-not (aph/equal 'foo     "foo"))
  (should-not (aph/equal '(1 1 0) [1 1 0] [1 1 0])))


(provide 'aph-dash-test)
