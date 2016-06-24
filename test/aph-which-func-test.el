;;; aph-which-func-test.el --- Tests for aph-which-func.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Depenencies: `aph-which-func', `proctor'

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

(require 'aph-which-func)
(require 'proctor)


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
  (proctor-with-buffer 'org-mode "
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
;;; aph-which-func-test.el ends here
