;;; aph-simple-test.el --- Tests for aph-simple.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-simple', `ert'

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

(require 'aph-simple)
(require 'ert)


;;;; Motion Commands
;;==================
(ert-deftest aph/simple-test-move-bol ()
  "Test `aph/move-beginning-of-line'."
  (with-temp-buffer
    (insert "foo")                                        ; "foo|"
    (should (= 1 (aph/move-beginning-of-line) (point)))   ; "|foo"
    (insert "  ")                                         ; "  |foo"
    (should (= 1 (aph/move-beginning-of-line) (point)))   ; "|  foo"
    (should (= 3 (aph/move-beginning-of-line) (point)))   ; "  |foo"
    (open-line 1)                                         ; "  |"/"foo"
    (should (= 4 (aph/move-beginning-of-line 2) (point))) ; "  "/"|foo"
    ))
      
(provide 'aph-simple-test)
;;; aph-simple-test.el ends here
