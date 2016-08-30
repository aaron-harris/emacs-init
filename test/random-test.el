;;; random-test.el --- Tests for random.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `random', `proctor'

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

(require 'random)
(require 'proctor)


;;;; Random dispatch
;;==================
(ert-deftest random-dispatch-test ()
  "Test `random-dispatch' macro."
  (proctor-random 10000 100
      ((:foo . 2500) (:bar . 5000) (:baz . 2500))
    (random-dispatch
     (1  :foo)
     (2  :foo :bar)
     (1  :baz))))

(provide 'random-test)
;;; random-test.el ends here
