;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; OUTLINE TESTS
;;;;============================================================================

;; Tests for the module aph-outline.el.
(require 'aph-outline)


;;; Testing Apparatus
;;;==================
(defmacro aph/outline-test (text &rest body)
  "Execute BODY in `outline-mode' buffer containing TEXT."
  (declare (indent 1)
           (debug (stringp body)))
  `(with-temp-buffer
     (insert ,text)
     (outline-mode)
     (goto-char (point-min))
     ,@body))


;;; Information Functions
;;;======================
(ert-deftest aph/outline-test-before-first-heading-p ()
  "Test `aph/outline-before-first-heading-p'." 
  (aph/outline-test
    "
Preamble
* Heading 1
More text" 
    (should (aph/outline-before-first-heading-p))
    (while (and (aph/outline-before-first-heading-p) (not (eobp)))
      (forward-char))
    (should (looking-at-p "* Heading 1"))
    (while (not (or (aph/outline-before-first-heading-p) (eobp)))
      (forward-char))
    (should (eobp))))

      
(provide 'aph-outline-test)
