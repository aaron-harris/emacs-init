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
|    |  15 |"
    (forward-line 2)
    (forward-char 2)
    (should (looking-at-p " 1 |"))
    (aph/org-increase-number 5)
    (org-table-beginning-of-field 1)
    (should (looking-at-p "6 |"))
    (forward-line 1) 
    (should (looking-at-p "| +| +15"))
    (should-error (aph/org-increase-number))
    (should (looking-at-p " +| +15"))))


(provide 'aph-org-test)
