;;; proctor-org-test.el --- Tests for proctor-org.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `proctor-org', `bfw'

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

(require 'bfw)


;;;; Agenda
;;=========
(ert-deftest proctor-org-test-agenda ()
  "Test `proctor-org-with-agenda-items'."
  (let ((agendas (proctor-org-list-agendas)))
    (proctor-org-with-agenda-items
        ("* TODO Foo"
         "* TODO Bar\n  More text"
         "* DONE Baz") 
      (org-todo-list "TODO")
      (should (search-forward "TODO Foo"))
      (goto-char (point-min))
      (should (search-forward "TODO Bar"))
      (goto-char (point-min))
      (should-not (search-forward "DONE Baz" nil :noerror)))
    (should (equal agendas (proctor-org-list-agendas)))
    (should-not (bfw-get-buffer-for-file proctor-org-temp-agenda-file))))

(ert-deftest proctor-org-test-agenda:timestamps ()
  "Test timestamp escapes in `proctor-org-with-agenda-items'."
  (proctor-org-with-agenda-items
      ("* Foo\n  %t"
       "* Bar\n  %T"
       "* Baz\n  %u"
       "* Quux\n  %U")
    (let* ((ct   (current-time))
           (v-t  (format-time-string (car org-time-stamp-formats) ct))
           (v-T  (format-time-string (cdr org-time-stamp-formats) ct))
           (v-u  (concat "[" (substring v-t 1 -1) "]"))
           (v-U  (concat "[" (substring v-T 1 -1) "]")))
      (with-temp-buffer
        (insert-file-contents proctor-org-temp-agenda-file)
        (should (search-forward (format "* Foo\n  %s"  v-t)))
        (should (search-forward (format "* Bar\n  %s"  v-T)))
        (should (search-forward (format "* Baz\n  %s"  v-u)))
        (should (search-forward (format "* Quux\n  %s" v-U)))))))

(provide 'proctor-org-test)
;;; proctor-org-test.el ends here
