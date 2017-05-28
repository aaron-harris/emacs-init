;;; aph-seq-test.el --- Tests for aph-seq.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-seq', `ert'

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

(require 'aph-seq)
(require 'ert)

(ert-deftest aph/seq-test-type ()
  "Test `aph/seq-type'."
  (proctor-test-all #'aph/seq-type #'eq
    (((1 2 3)) . list)
    (([1 2 3]) . vector)
    (("Foo")   . string)
    ((nil)     . list))
  (should-error (aph/seq-type 3) :type 'wrong-type-argument))

(ert-deftest aph/seq-test-reductions ()
  "Test `aph/seq-reductions'."
  (proctor-test-all #'aph/seq-reductions #'equal
    ((+ (1 2 3) 4) . (4 5 7 10))))

(ert-deftest aph/seq-test-successor ()
  "Test `aph/seq-successor'."
  (proctor-test-all #'aph/seq-successor #'eq
    (((a b c) a)       . b)
    (((a b c) a nil t) . b)
    (((a b c) b)       . c)
    (((a b c) c)       . nil)
    (((a b c) c nil t) . a)
    (((a b c) d)       . nil)
    (((a b c) d nil t) . nil)
    ((nil     a)       . nil)))

(provide 'aph-seq-test)
;;; aph-seq-test.el ends here
