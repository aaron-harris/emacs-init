;;; alist-test.el --- Tests for alist.el             -*- lexical-binding: t; -*-

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

(require 'alist)
(require 'proctor)


;;;; Updating
;;===========
(ert-deftest alist-test-delete ()
  "Test `alist-delete'."
  (let ((alist  (list '(foo . 1) '(bar . 2) '(1 . 3) '(1.0 . 4))))
    (proctor-test-all
        (lambda (key)
          (alist-delete alist key))
        (lambda (expected result)
          (and (equal expected alist)
               (equal expected result)))
      ((foo) . ((bar . 2) (1 . 3) (1.0 . 4))))))


;;;; Equality Testing
;;===================
(ert-deftest alist-test-equal ()
  "Test `alist-equal'."
  (proctor-test-all
      #'alist-equal
      #'eq
    ;; alist1     alist2       extras     . expected
    ((nil         nil)                    . t)
    ((((foo . 1)) ((foo . 1)))            . t)
    ((((foo . 1)) ((foo . 2)))            . nil)
    ((((foo . 1)) nil)                    . nil)
    ((((foo . 1)) nil          nil nil 1) . t)
    ((nil         ((foo . 1)))            . nil)))

(provide 'alist-test)
;;; alist-test.el ends here
