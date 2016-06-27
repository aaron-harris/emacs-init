;;; populate-test.el --- Tests for populate.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `populate', `proctor'

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

(require 'populate)
(require 'proctor)

(ert-deftest populate-test-down ()
  "Test `populate-downwards-in-region'."
  (proctor-with-buffer 'fundamental-mode "

Foo
\t
\s\s
Bar"
    (populate-downwards-in-region (point-min) (point-max))
    (should (equal (buffer-string)
		   (concat "\n"
			   "Foo\n"
			   "Foo\n"
			   "Foo\n"
			   "Bar")))))

(provide 'populate-test)
;;; populate-test.el ends here
