;;; org-agenda-count-test.el --- Tests for org-agenda-count.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `org-agenda-count', `proctor-org'

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

(require 'org-agenda-count)
(require 'proctor-org)

(ert-deftest org-agenda-count-test ()
  "Test `org-agenda-count'."
  (proctor-org-with-agenda-items
   ("* TODO Foo 1  :foo:"
    "* TODO Foo 2  :foo:"
    "* TODO Bar 1  :bar:"
    "* TODO Foo 3  :foo:"
    "* TODO Bar 2  :bar:"
    "* TODO Foo 4  :foo:")
   (let ((org-agenda-custom-commands
          '(("x" "Test agenda"
             ((tags-todo
               "foo"
               ((org-agenda-overriding-header
                 (format "Foo [%s]" (org-agenda-count "foo")))
                (org-agenda-max-entries 3)))
              (tags-todo
               "bar"
               ((org-agenda-overriding-header
                 (format "Bar [%s]" (org-agenda-count "bar")))))
              (tags-todo
               "baz"
               ((org-agenda-overriding-header
                 (format "Baz [%s]" (org-agenda-count "baz"))))))))))
     (org-agenda nil "x")
     (should (search-forward "Foo [4]" nil :noerror))
     (should (search-forward "Bar [2]" nil :noerror))
     (should (search-forward "Baz [0]" nil :noerror)))))

(provide 'org-agenda-count-test)
;;; org-agenda-count-test.el ends here
