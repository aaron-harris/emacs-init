;;; proctor-test.el --- Tests for proctor.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `proctor'

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

(require 'proctor)


;;;; Macro Testing
;;================
(ert-deftest proctor-test-macro-executes-body ()
  "Test `proctor-macro-executes-body'." 
  (should (proctor-macro-executes-body 'with-temp-buffer))
  (should (proctor-macro-executes-body 'let '((canary))))
  (should-error (proctor-macro-executes-body 'ignore)))

(ert-deftest proctor-test-macro-does-not-leak ()
  "Test `proctor-macro-does-not-leak-p'."
  (should (proctor-macro-does-not-leak 'let 'var-x '((var-x))))
  (should-error (proctor-macro-does-not-leak-p
                 'let ''emacs-version '((var-x)))))


;;;; Buffer Handling
;;==================
(ert-deftest proctor-test-with-buffer ()
  "Test `proctor-with-buffer'."
  (should (proctor-macro-executes-body 'proctor-with-buffer
                                       '('text-mode "Foo")))
  (dolist (text '("Foo" "\nFoo"))
    (proctor-with-buffer 'text-mode text
      (should (eq major-mode 'text-mode))
      (should (looking-at-p "Foo")))))

(provide 'proctor-test)
;;; proctor-test.el ends here
