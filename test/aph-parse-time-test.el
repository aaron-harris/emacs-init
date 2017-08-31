;;; aph-parse-time-test.el --- Tests for aph-parse-time.el -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Aaron Harris

;; Author: Aaron Harris <aaron.patrick.harris@gmail.com>

;; Dependencies: `aph-parse-time', `proctor'

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

(require 'aph-parse-time)
(eval-when-compile (require 'cl-lib))

(require 'proctor)

(ert-deftest aph/test-parse-time-string-and-format ()
  "Test `aph/parse-time-string-and-format'."
  (proctor-test-all #'aph/parse-time-string-and-format #'equal
    (("%D" "May 20, 2017") . "05/20/17")
    (("%D %R" "May 20, 2017 9:30") . "05/20/17 09:30")))

(provide 'aph-parse-time-test)
;;; aph-parse-time-test.el ends here
