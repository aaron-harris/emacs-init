;;; formation-test.el --- Tests for formation.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: 

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

(require 'formation)
(require 'proctor-forms)

(ert-deftest formation-test-map ()
  "Test `formation-map'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "bar"))
    (forms-first-record)
    (should (equal (formation-map (lambda () (nth 2 forms-fields)))
                   '("foo" "bar")))
    (should (= forms--current-record 1))))

(ert-deftest formation-test-map:filter ()
  "Test `formation-map' with filter arg."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "bar"))
    (should (equal (formation-map (lambda () (nth 1 forms-fields))
                                  (lambda () (equal "foo" (nth 2 forms-fields))))
                   '("1")))))

(provide 'formation-test)
;;; formation-test.el ends here
