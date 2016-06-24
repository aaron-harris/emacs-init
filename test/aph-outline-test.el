;;; jerk-test.el --- Tests for aph-outline.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-outline'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'aph-outline)
(require 'proctor)


;;;; State Wrappers
;;=================
(ert-deftest aph/outline-test-before-first-heading ()
  "Test `aph/outline-before-first-heading'."
  (proctor-with-buffer 'outline-mode "
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
  (proctor-with-buffer 'outline-mode "
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


;;;; General Motion
;;=================
(ert-deftest aph/outline-*-heading ()
  "Test `aph/outline-next-heading', `aph/outline-previous-heading'."
  (dolist (invis '(nil t))
    (proctor-with-buffer 'outline-mode "
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


;;;; Lateral Motion
;;=================
(ert-deftest aph/outline-test-get-*-sibling ()
  "Test `aph/outline--get-*-sibling' and related commands.
The specific commands tested are `aph/outline-get-first-sibling'
and `aph/outline-get-final-sibling'."
  (proctor-with-buffer 'outline-mode "
Preamble
* Heading 1
** Subheading 1a
* Heading 2
* Heading 3
** Subheading 3a"
    (let* ((subtest
            (lambda (f expected)
              (if expected
                  (dolist (loud '(nil t))
                    (save-excursion
                      (should (eq (funcall f loud) (point)))
                      (should (looking-at-p expected))))
                (let ((pos (point)))
                  (should (null (funcall f nil)))
                  (should (eq pos (point)))
                  (should-error (funcall f :loud))
                  (should (eq pos (point)))))))
           (test
            (lambda (start-hdg back-hdg fwd-hdg)
              (should (looking-at-p start-hdg))
              (funcall subtest #'aph/outline-get-first-sibling back-hdg)
              (funcall subtest #'aph/outline-get-final-sibling fwd-hdg))))
      (funcall test
               "Preamble"
               nil
               nil)
      (aph/outline-next-heading 1 :invisible-ok)
      (funcall test
               "* Heading 1"
               nil
               "* Heading 3")
      (aph/outline-get-next-sibling)
      (message "Currently at: %d" (point))
      (funcall test
               "* Heading 2"
               "* Heading 1"
               "* Heading 3")
      (aph/outline-get-next-sibling)
      (funcall test
               "* Heading 3"
               "* Heading 1"
               nil))))


;;;; Vertical Motion
;;==================
(ert-deftest aph/outline-test-top-heading ()
  "Test `aph/outline-top-heading'."
  (dolist (invis '(nil t))
    (proctor-with-buffer 'outline-mode "
Preamble
* Heading 1
** Subheading 1a"
      (let ((test
             (lambda (start-hdg end-hdg)
               (save-excursion
                 (should (looking-at-p start-hdg))
                 (should (eq (aph/outline-top-heading invis) (point)))
                 (should (looking-at-p end-hdg))))))
        (should-error (aph/outline-top-heading))
        (aph/outline-next-heading 1 invis)
        (should-error (aph/outline-top-heading)) 
        (aph/outline-next-heading 1 invis)
        (funcall test "** Subheading 1a" "* Heading 1")))))

(ert-deftest aph/outline-test-get-first-child ()
  "Test `aph/outline-get-first-child'."
  (proctor-with-buffer 'outline-mode "
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
  (proctor-with-buffer 'outline-mode "
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
  (proctor-with-buffer 'outline-mode "
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
;; aph-outline-test.el ends here
