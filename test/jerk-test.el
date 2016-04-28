;;; jerk-test.el --- Tests for jerk.el               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `jerk', `ert'

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

(require 'jerk)
(require 'ert)

(ert-deftest jerk-test-access ()
  "Test `jerk-access'."
  (let (kill-ring)
    (with-temp-buffer
      (kill-new "Alpha\tBravo\nFoo\tBar")
      (should (equal "Foo\tBar"
                     (jerk-access)))
      (should (equal "Foo\tBar"
                     (buffer-string)))
      (should (equal '("Foo\tBar" "Alpha\tBravo\nFoo\tBar")
                     kill-ring)))))


(provide 'jerk-test)
;;; jerk-test.el ends here
