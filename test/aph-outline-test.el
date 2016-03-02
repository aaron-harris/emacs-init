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
(ert-deftest aph/outline-test-before-first-heading ()
  "Test `aph/outline-before-first-heading'."
  (aph/outline-test
    "
Preamble
* Heading 1
More text" 
    (should (aph/outline-before-first-heading))
    (while (and (aph/outline-before-first-heading) (not (eobp)))
      (forward-char))
    (should (looking-at-p "* Heading 1"))
    (should (outline-level))            ; Check that match data is OK
    (while (not (or (aph/outline-before-first-heading) (eobp)))
      (forward-char))
    (should (eobp))))

      
(provide 'aph-outline-test)
