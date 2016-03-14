;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG TESTS
;;;;============================================================================

;; Tests for the module aph-org.el.
(require 'aph-org)


;;; Editing Functions
;;;==================
(ert-deftest aph/org-increase-number ()
  "Test `aph/org-increase-number'."
  (aph/ert-with-buffer 'org-mode "
| A  | B   |
|----+-----|
|  1 | foo |
| 15 | bar |"
    (forward-line 2)
    (forward-char 2)
    (should (looking-at-p " 1 |"))
    (aph/org-increase-number 5)
    (org-table-beginning-of-field 1)
    (should (looking-at-p " 6 |"))))


(provide 'aph-org-test)
