;;; helm-forms-test.el --- Tests for helm-forms.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

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

(require 'helm-forms)
(require 'proctor-forms)
(require 'proctor-helm)


;;;; Commands
;;===========
(ert-deftest helm-forms-test-records ()
  "Test `helm-forms-records'."
  (proctor-forms-with-db nil 
      (("foo" "bar") ("baz" "quux"))
    (proctor-with-helm (helm-forms-records)
      (should (= 2 (helm-get-candidate-number))))))

(ert-deftest helm-forms-test-narrowing ()
  "Test `helm-forms-records' with `forms-narrow'."
  (proctor-forms-with-db nil
      (("foo" "bar") ("baz" "quux"))
    (forms-narrow-regexp "baz")
    (proctor-with-helm (helm-forms-records)
      (should (= 1 (helm-get-candidate-number))))))

(provide 'helm-forms-test)
;;; helm-forms-test.el ends here
