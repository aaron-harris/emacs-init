;;; filet-test.el --- Tests for filet.el             -*- lexical-binding: t; -*-

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

;;; Commentary:

;; 

;;; Code:

(require 'filet)
(require 'aph-ert)


;;; Basic Tests
;;;============
(ert-deftest filet-test-basic ()
  "Test basic functionality of `filet'."
  (let (kill-ring)
    (with-temp-buffer
      (kill-new "Foo")
      (should (equal "Foo"
                     (filet #'identity)))
      (should (equal "Foobar"
                     (filet (lambda (kill) (concat kill "bar")))))
      (should (equal kill-ring
                     '("Foobar" "Foo" "Foo"))))))

(provide 'filet-test)
;;; filet-test.el ends here
