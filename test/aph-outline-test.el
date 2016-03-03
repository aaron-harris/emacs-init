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
  (aph/outline-test "
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

(ert-deftest aph/outline-test-level ()
  "Test `aph/outline-level'."
  (aph/outline-test "
Preamble
* Heading 1
Text in H1"
    (should (zerop (aph/outline-level)))
    (outline-next-heading)
    (should (looking-at-p "* Heading 1"))
    (should (= (aph/outline-level) (funcall outline-level))) 
    (should (= (funcall outline-level)
               (progn (forward-line)
                      (aph/outline-level))))))


;;; Navigation Functions
;;;=====================
(ert-deftest aph/outline-*-heading ()
  "Test `aph/outline-next-heading', `aph/outline-previous-heading'."
  (dolist (invis '(nil t))
    (aph/outline-test "
Preamble
* Heading 1
** Subheading 1a
* Heading 2"
      (aph/outline-next-heading 1 invis)
      (should (looking-at-p "* Heading 1"))
      (aph/outline-next-heading 2 invis)
      (should (looking-at-p "* Heading 2"))
      (aph/outline-next-heading -1 invis)
      (should (looking-at-p "** Subheading 1a"))
      (aph/outline-previous-heading -1 invis)
      (should (looking-at-p "* Heading 2"))
      (aph/outline-previous-heading 2 invis)
      (should (looking-at-p "* Heading 1"))
      (aph/outline-previous-heading 1 invis)
      (should (bobp)))))

(ert-deftest aph/outline-test-get-first-child ()
  "Test `aph/outline-get-first-child'."
  (aph/outline-test "
Preamble
* Heading 1
** Subheading 1a
*** Subsubheading 1a.1
** Subheading 1b"
    (should (eq (aph/outline-get-first-child) (point)))
    (should (looking-at-p "* Heading 1"))
    (should (eq (aph/outline-get-first-child) (point)))
    (should (looking-at-p "** Subheading 1a"))
    (should (eq (aph/outline-get-first-child) (point)))
    (should (looking-at-p "*** Subsubheading 1a.1"))
    (should (null (aph/outline-get-first-child)))
    (should (looking-at-p "*** Subsubheading 1a.1"))))

(ert-deftest aph/outline-test-down-heading ()
  "Test `aph/outline-down-heading'."
  (aph/outline-test "
Preamble
* Heading 1
** Subheading 1a
*** Subsubheading 1a.1
** Subheading 1b"
    (aph/outline-down-heading 1)
    (should (looking-at-p "* Heading 1"))
    (aph/outline-down-heading 2)
    (should (looking-at-p "*** Subsubheading 1a.1"))
    (aph/outline-down-heading -1)
    (should (looking-at-p "** Subheading 1a"))
    (aph/outline-down-heading 5)
    (should (looking-at-p "*** Subsubheading 1a.1"))))

(ert-deftest aph/outline-test-down-heading-from-end ()
  "Test `aph/outline-down-heading-from-end'."
  (aph/outline-test "
Preamble
* Heading 1
** Subheading 1a
* Heading 2
** Subheading 2a
** Subheading 2b
*** Subsubheading 2b.1"
    (aph/outline-down-heading-from-end 1)
    (should (looking-at-p "* Heading 2"))
    (aph/outline-down-heading-from-end 1)
    (should (looking-at-p "** Subheading 2b"))
    (aph/outline-down-heading-from-end -1)
    (should (looking-at-p "* Heading 2"))
    (aph/outline-down-heading-from-end 3)
    (should (looking-at-p "*** Subsubheading 2b.1"))))

      
(provide 'aph-outline-test)
