;;; atlas-test.el --- Tests for atlas.el             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `atlas', `proctor'

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

(require 'atlas)
(require 'proctor)

(ert-deftest atlas-test-map-lines ()
  "Test `atlas-map-lines'"
  (proctor-with-buffer 'text-mode "
foo
bar"
    (atlas-map-lines
     (lambda (line) (concat line "?"))
     (point-min) (point-max))
    (should (equal (buffer-string) "foo?\nbar?"))))

(provide 'atlas-test)
;;; atlas-test.el ends here
