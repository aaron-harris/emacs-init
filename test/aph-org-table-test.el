;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG TABLE TESTS
;;;;============================================================================

;; Tests for the module aph-org-table.el.
(require 'aph-org-table)


;;; Editing Functions
;;;==================
(ert-deftest aph/org-table-test-clear-row-forward ()
  "Test `aph/org-table-clear-row-forward'."
  (aph/ert-with-buffer 'org-mode "
| A | B     | C     |
|---+-------+-------|
| 1 | foo   | bar   |
| 2 | alpha | bravo |"
    (next-line 2)
    (forward-char 3)
    (should (looking-at-p " | foo"))
    (aph/org-table-clear-row-forward)
    (should (looking-at-p " |       |       |"))))

      
(provide 'aph-org-table-test)
