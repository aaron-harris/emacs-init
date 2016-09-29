;;; aph-projectile-test.el --- Tests for aph-projectile.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

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

(require 'aph-projectile)
(require 'proctor)

(ert-deftest aph/projectile-test-prefix/suffix ()
  "Test `aph/projectile-test-*' functions.
These are `aph/projectile-test-prefix' and
`aph/projectile-test-suffix'."
  (let* ((project-foo  (make-symbol "foo"))
         (aph/projectile-test-alist
          `((,project-foo . ("foo-"  . "-foo"))
            (emacs-cask   . ("cask-" . nil))
            (django       . (nil     . "-django")))))
    (proctor-test-all #'aph/projectile-test-prefix #'equal
      ((,project-foo) . "foo-")
      ((emacs-cask)   . "cask-")
      ((django)       . ,(projectile-test-prefix 'django)))
    (proctor-test-all #'aph/projectile-test-suffix #'equal
      ((,project-foo) . "-foo")
      ((emacs-cask)   . ,(projectile-test-suffix 'emacs-cask))
      ((django)       . "-django"))))


(provide 'aph-projectile-test)
;;; aph-projectile-test.el ends here
