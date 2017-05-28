;;; cygwinize-test.el --- Tests for cygwinize.el     -*- lexical-binding: t; -*-

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

(ert-deftest cygwinize-test-hybrid-conversion ()
  "Test `cygwinize-convert-file-name-to-hybrid-windows'."
  (let* ((root (cygwinize-convert-file-name-to-hybrid-windows "/"))
         (home (cygwinize-convert-file-name-to-hybrid-windows "~")))
    (should (stringp root))
    (should (equal (cygwinize-convert-file-name-to-hybrid-windows
                    "/foo/bar/baz.el")
                   (concat root "/foo/bar/baz.el")))
    (should (equal (cygwinize-convert-file-name-to-hybrid-windows
                    "~/foo/bar/baz.el")
                   (concat home "/foo/bar/baz.el")))))

(provide 'cygwinize-test)
;;; cygwinize-test.el ends here
