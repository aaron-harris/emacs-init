;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; WHICH-FUNCTION-MODE TESTS
;;;;============================================================================

;; Tests for the module `aph-which-func'.
(require 'aph-which-func)
(require 'aph-ert)


;;; Org Mode Support Tests
;;;=======================
(defun aph/which-function-test-org-1 (here expected)
  "Test `aph/which-function-org' at current position.
Test (using `should') that we are `looking-at-p' the string HERE
and that `aph/which-function-org' returns EXPECTED."
  (should (looking-at-p here))
  (should (equal (aph/which-function-org) expected)))

(ert-deftest aph/which-function-test-org ()
  "Test `aph/which-function-org'."
  (aph/ert-with-buffer 'org-mode "
Preface
* Heading 1
Text under Heading 1"
    (should (aph/which-function-test-org-1 "Preface"
                                           "-----")) 
    (org-next-visible-heading 1)
    (should (aph/which-function-test-org-1 "* Heading 1"
                                           "Heading 1")) 
    (forward-line)
    (should (aph/which-function-test-org-1 "Text under Heading 1"
                                           "Heading 1"))))


(provide 'aph-which-func-test)
