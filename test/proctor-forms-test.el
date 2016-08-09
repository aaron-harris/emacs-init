;;; proctor-forms-test.el --- Tests for proctor-forms.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `proctor-forms'

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

(require 'proctor-forms)


;;;; Fixtures
;;===========
(ert-deftest proctor-forms-test-db ()
  "Test `proctor-forms-with-db'."
  (proctor-macro-executes-body
   'proctor-forms-with-db
   '(nil (("1" "foo") ("2" ""))))
  (let (buffer)
    (proctor-forms-with-db
        (setq forms-number-of-fields 2
              forms-format-list      '("Test format"))
        (("1" "foo") ("2" ""))
      (should (eq major-mode 'forms-mode))
      (should (equal (buffer-string) "Test format")) 
      (should (equal forms-fields '(nil "2" "")))
      (should (= 2 forms--total-records))
      (forms-first-record)
      (should (equal forms-fields '(nil "1" "foo")))
      (setq buffer (current-buffer)))
    (should-not (buffer-live-p buffer))))

(provide 'proctor-forms-test)
;;; proctor-forms-test.el ends here
