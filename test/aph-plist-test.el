;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; PLIST TESTS
;;;;============================================================================

;; Tests for the module `aph-plist'.
(require 'aph-plist)

(ert-deftest aph/plist-test-get-as-list ()
  "Test `aph/plist-get-as-list'."
  (require 'aph-dash)                   ; For `aph/equal'
  (should (aph/equal (aph/plist-get-as-list '(:foo 1) :foo)
                     (list (plist-get '(:foo 1) :foo))
                     '(1)))
  (should (aph/equal (aph/plist-get-as-list '(:foo (1 2)) :foo)
                     (plist-get '(:foo (1 2)) :foo)
                     '(1 2)))
  (should (aph/equal (aph/plist-get-as-list '(:foo 1) :bar)
                     (plist-get '(:foo 1) :bar)
                     nil))
  (should (aph/equal (aph/plist-get-as-list nil :foo)
                     (plist-get nil :foo)
                     nil)))


(provide 'aph-ert-test)
