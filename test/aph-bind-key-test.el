;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; BIND-KEY TESTS
;;;;============================================================================

;; Tests for the module `aph-bind-key'.
(require 'aph-bind-key)
(require 'aph-ert)


;;; Testing Apparatus
;;;==================



;;; Tests for `describe-personal-keybindings' fixes
;;;================================================
(ert-deftest aph/bind-key-test-get-binding-description-advice--menu-item ()
  "Test `aph/bind-key-get-binding-description-advice--menu-item'."
  (let ((personal-keybindings nil)
        (test-menu-item '(menu-item "foo" nil :filter ignore)))
    (equal (aph/bind-key-get-binding-description-advice--menu-item
            test-menu-item)
           "foo")
    (aph/ert-with-minor-mode foo-mode
      (eval `(bind-key "a" ',test-menu-item ,(aph/symbol-concat foo-mode "-map")))
      (should (aph/ert-protecting-buffer "*Personal Keybindings*"
                (describe-personal-keybindings)
                :success)))))


(provide 'aph-bind-key-test)
