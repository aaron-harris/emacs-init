;;; enumerate-test.el --- Tests for enumerate.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Dependencies: `enumerate', `ert'

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

(require 'enumerate)
(require 'ert)


;;; Subroutine Tests
;;;=================
(ert-deftest enumerate-test-bol/-eol ()
  "Test functions `enumerate--bol' and `enumerate--eol'."
  (with-temp-buffer
    (should (= (enumerate--bol) (enumerate--bol (point))
               (enumerate--eol) (enumerate--eol (point))
               (point) (point-min) (point-max)))
    (insert "A")
    (should (= (enumerate--bol) (enumerate--bol (point)) 1))
    (should (= (enumerate--eol) (enumerate--eol (point)) 2 (point)))
    (insert "B") 
    (should (= (enumerate--bol) (enumerate--bol (point)) 1))
    (should (= (enumerate--eol) (enumerate--eol (point)) 3 (point)))
    (insert "\n")
    (should (= (enumerate--bol 1) (enumerate--bol 3) 1))
    (should (= (enumerate--eol 1) (enumerate--eol 3) 3)) 
    (should (= (enumerate--bol) (enumerate--bol (point))
               (enumerate--eol) (enumerate--eol (point)) 4 (point)))
    (insert "foo\n\nbar")
    (should (= (enumerate--bol 4) (enumerate--bol 6) 4))
    (should (= (enumerate--eol 4) (enumerate--eol 6) 7))
    (should (= (enumerate--bol 8) (enumerate--eol 8) 8))
    (should (= (enumerate--bol 9) (enumerate--bol) 9))
    (should (= (enumerate--eol 9) (enumerate--eol) 12 (point)))))


;;; Command Tests
;;;==============
(ert-deftest enumerate-test-lines ()
  "Test `enumerate-lines'."
  (with-temp-buffer
    (enumerate-lines (point-min) (point-max))
    (should (equal "1 " (buffer-string))))
  (with-temp-buffer
    (insert "A\nB\nC")
    (enumerate-lines (point-min) (point-max))
    (should (equal "1 A\n2 B\n3 C" (buffer-string))))
  (with-temp-buffer
    (insert (make-string 99 ?\n))
    (enumerate-lines (point-min) (point-max))
    (should (equal (buffer-string)
                   (mapconcat (apply-partially #'format "%3d ")
                              (number-sequence 1 100)
                              "\n")))))

(ert-deftest enumerate-test-alpha ()
  "Test `enumerate-alpha'."
  (with-temp-buffer
    (insert "C\nB\nA")
    (enumerate-alpha (point-min) (point-max))
    (should (equal "3 C\n2 B\n1 A"
                   (buffer-string)))))

(provide 'enumerate-test)
;;; enumerate-test.el ends here
