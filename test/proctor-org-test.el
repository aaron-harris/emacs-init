;;; proctor-org-test.el --- Tests for proctor-org.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `proctor-org'

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

(require 'proctor-org)


;;;; Agenda
;;=========
(ert-deftest proctor-org-test-agenda ()
  "Test `proctor-org-with-agenda'."
  (let ((agendas (proctor-org-list-agendas)))
    (proctor-org-with-agenda-items
        ("* TODO Foo"
         "* TODO Bar\n  More text"
         "* DONE Baz") 
      (org-todo-list "TODO")
      (should (re-search-forward "TODO Foo"))
      (goto-char (point-min))
      (should (re-search-forward "TODO Bar"))
      (goto-char (point-min))
      (should-not (re-search-forward "DONE Baz" nil :noerror)))
    (should (equal agendas (proctor-org-list-agendas)))))

(provide 'proctor-org-test)
;;; proctor-org-test.el ends here
