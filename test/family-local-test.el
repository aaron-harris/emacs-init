;;; family-local-test.el --- Tests for family-local.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `family-local', `aph-ert', `mode-family-test'

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

(require 'family-local)
(require 'aph-ert)			; For `aph/ert-with-major-mode'
(require 'mode-family-test)		; For `mode-family-test' macro


;;;; Testing Apparatus
;;====================
(defmacro family-local-test (&rest body)
  "As `mode-family-test', and also save family local variables."
  (declare (debug body))
  `(let (family-local--alist)
     (mode-family-test ,@body)))


;;;; Tests
;;========
(ert-deftest family-local-test ()
  "Test family-local variables."
  (family-local-test
   (mode-family-create 'foo)
   (setq-family-local foo
     case-fold-search :foo)
   (aph/ert-with-major-mode mode 'fundamental-mode
     (mode-family-add mode 'foo)
     (with-temp-buffer
       (funcall mode)
       (should (eq case-fold-search :foo))
       (text-mode)
       (should-not (eq case-fold-search :foo))))))

(provide 'family-local-test)
;;; family-local-test.el ends here
