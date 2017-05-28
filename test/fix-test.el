;;; fix-test.el --- Tests for fix.el                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `fix', `proctor'

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

(require 'fix)
(require 'proctor)


;;;; Subroutines
;;==============
(ert-deftest fix-test-name ()
  "Test `fix--name'."
  (should (equal (fix--name 'foo "bar") 'foo:bar)))


;;;; Partial Application
;;======================
(ert-deftest fix-test-args ()
  "Test `fix-args'."
  (let ((fun-name (fix--name 'format "foo")))
    (unwind-protect
        (progn
          (should (eq fun-name (fix-args #'format "foo" "%s %s" "foo")))
          (should (fboundp fun-name))
          (should (equal (funcall fun-name "bar") "foo bar")))
      (fmakunbound fun-name))))


;;;; Mode Switches
;;================
(ert-deftest fix-test-mode-on/off ()
  "Test `fix-mode-on', `fix-mode-off'."
  (proctor-with-minor-mode foo-mode
    (let ((on-name  (fix--name foo-mode "on"))
          (off-name (fix--name foo-mode "off")))
      (unwind-protect
          (progn
            (should (eq on-name (fix-mode-on foo-mode)))
            (funcall on-name)
            (should (symbol-value foo-mode))
            (should (eq off-name (fix-mode-off foo-mode)))
            (funcall off-name)
            (should-not (symbol-value foo-mode)))
        (fmakunbound on-name)
        (fmakunbound off-name)))))

(provide 'fix-test)
;;; fix-test.el ends here
