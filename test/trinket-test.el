;;; trinket-test.el --- Tests for trinket.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `trinket', `ert'

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

(require 'trinket)
(require 'ert)

(ert-deftest trinket-test-bol/-eol ()
  "Test functions `trinket-bol' and `trinket-eol'."
  (with-temp-buffer
    (should (= (trinket-bol) (trinket-bol (point))
               (trinket-eol) (trinket-eol (point))
               (point) (point-min) (point-max)))
    (insert "A")
    (should (= (trinket-bol) (trinket-bol (point)) 1))
    (should (= (trinket-eol) (trinket-eol (point)) 2 (point)))
    (insert "B") 
    (should (= (trinket-bol) (trinket-bol (point)) 1))
    (should (= (trinket-eol) (trinket-eol (point)) 3 (point)))
    (insert "\n")
    (should (= (trinket-bol 1) (trinket-bol 3) 1))
    (should (= (trinket-eol 1) (trinket-eol 3) 3)) 
    (should (= (trinket-bol) (trinket-bol (point))
               (trinket-eol) (trinket-eol (point)) 4 (point)))
    (insert "foo\n\nbar")
    (should (= (trinket-bol 4) (trinket-bol 6) 4))
    (should (= (trinket-eol 4) (trinket-eol 6) 7))
    (should (= (trinket-bol 8) (trinket-eol 8) 8))
    (should (= (trinket-bol 9) (trinket-bol) 9))
    (should (= (trinket-eol 9) (trinket-eol) 12 (point)))))

(provide 'trinket-test)
;;; trinket-test.el ends here
