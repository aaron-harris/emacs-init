;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; OUTLINE TESTS
;;;;============================================================================

;; Tests for the module aph-outline.el.
(require 'aph-outline)


;;; Information Functions
;;;======================
(ert-deftest aph/outline-test-before-first-heading-p ()
  "Test `aph/outline-before-first-heading-p'."
  (with-temp-buffer
    (insert "Test contents
* Heading 1
More text")
    (outline-mode)
    (goto-char (point-min))
    (should (aph/outline-before-first-heading-p))
    (while (and (aph/outline-before-first-heading-p) (not (eobp)))
      (forward-char))
    (should (looking-at-p "* Heading 1"))
    (while (not (or (aph/outline-before-first-heading-p) (eobp)))
      (forward-char))
    (should (eobp))))

      
(provide 'aph-outline-test)
