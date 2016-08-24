;;; formation-test.el --- Tests for formation.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `formation', `proctor-forms', `forms-narrow' (optional)

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


;;;; `formation-reduce'
;;=====================
(defun formation-test-reducer (acc)
  "Reducer function to test `formation-reduce'.

Append the value of the first field in the current `forms-mode'
record to ACC, using a colon as a separator."
  (concat acc ":" (nth 1 forms-fields)))

(ert-deftest formation-test-reduce ()
  "Test `formation-reduce'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "bar") ("3" "baz") ("4" "foo"))
    (forms-jump-record 2)
    (proctor-test-all
        (lambda (acc)
          (formation-reduce #'formation-test-reducer acc))
        #'equal
      ((nil) . ":1:2:3:4")
      (("0") . "0:1:2:3:4"))
    (should (= forms--current-record 2))))

(ert-deftest formation-test-reduce:filter ()
  "Test `formation-reduce' with filter arguments."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "bar") ("3" "baz") ("4" "foo"))
    (proctor-test-all
        (lambda (filter) 
          (formation-reduce #'formation-test-reducer "0" filter))
        #'equal
      ((nil)                                            . "0:1:2:3:4")
      (((lambda () (equal "foo" (nth 2 forms-fields)))) . "0:1:4"))))

(ert-deftest formation-test-reduce:narrow ()
  "Test `formation-reduce' with `forms-narrow'."
  (skip-unless (require 'forms-narrow nil :noerror))
  (proctor-forms-with-db nil
      (("1" "foo" "A") ("2" "bar" "A") ("3" "baz" "A") ("4" "foo" "B"))
    (forms-narrow (lambda () (equal "A" (nth 3 forms-fields))))
    ;; `formation-reduce' should respect narrowing.
    (proctor-test-all
        (lambda (filter) 
          (formation-reduce #'formation-test-reducer "0" filter))
        #'equal
      ((nil)                                            . "0:1:2:3")
      (((lambda () (equal "foo" (nth 2 forms-fields)))) . "0:1"))
    ;; `formation-reduce-all' should ignore narrowing.
    (proctor-test-all
        (lambda (filter)
          (formation-reduce-all #'formation-test-reducer "0" filter))
        #'equal
      ((nil)                                            . "0:1:2:3:4")
      (((lambda () (equal "foo" (nth 2 forms-fields)))) . "0:1:4"))))


;;;; `formation-map'
;;==================
(defun formation-test-mapper ()
  "Mapping function to test `formation-map'."
  (nth 1 forms-fields))

(ert-deftest formation-test-map ()
  "Test `formation-map'."
  (proctor-forms-with-db nil
      (("1" "foo") ("2" "bar") ("3" "baz") ("4" "foo"))
    (forms-jump-record 2)
    (proctor-test-all
        (lambda (filter)
          (formation-map #'formation-test-mapper filter))
        #'equal
      ((nil)                                            . ("1" "2" "3" "4"))
      (((lambda () (equal "foo" (nth 2 forms-fields)))) . ("1" "4")))
    (should (= forms--current-record 2))))

(ert-deftest formation-test-map:narrow ()
  "Test `formation-map' with `forms-narrow'."
  (skip-unless (require 'forms-narrow nil :noerror))
  (proctor-forms-with-db nil
      (("1" "foo" "A") ("2" "bar" "A") ("3" "baz" "A") ("4" "foo" "B"))
    (forms-narrow (lambda () (equal "A" (nth 3 forms-fields))))
    ;; `formation-map' should respect narrowing.
    (proctor-test-all
        (lambda (filter)
          (formation-map #'formation-test-mapper filter))
        #'equal
      ((nil)                                            . ("1" "2" "3"))
      (((lambda () (equal "foo" (nth 2 forms-fields)))) . ("1")))
    ;; `formation-map-all' should ignore narrowing. 
    (proctor-test-all
        (lambda (filter)
          (formation-map-all #'formation-test-mapper filter))
        #'equal
      ((nil)                                            . ("1" "2" "3" "4"))
      (((lambda () (equal "foo" (nth 2 forms-fields)))) . ("1" "4")))))

(provide 'formation-test)
;;; formation-test.el ends here
