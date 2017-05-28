;;; aph-helm-test.el --- Tests for aph-helm.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-helm', `proctor', `proctor-helm'

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

(require 'aph-helm)

(require 'proctor)
(require 'proctor-helm)


;;;; Imenu
;;========
(ert-deftest aph/helm-test-semantic-or-imenu--show-all ()
  "Test that `aph/helm-semantic-or-imenu' shows all candidates."
  (proctor-with-buffer 'emacs-lisp-mode "
\(defun foo ()
  \"Foo doc\"
  (ignore))

\(defun bar ()
  \"Bar doc\"
  (foo))"
    (re-search-forward "(foo")
    (proctor-with-helm (aph/helm-semantic-or-imenu nil)
      (should (= 2 (helm-get-candidate-number))))))

(ert-deftest aph/helm-test-semantic-or-imenu--no-jump ()
  "Test that `aph/helm-semantic-or-imenu' doesn't auto-jump."
  (let (pos)
    (proctor-with-buffer 'emacs-lisp-mode "
\(defun foo ()
  \"Foo doc\"
  (ignore))

\(foo)"
      (re-search-forward "(foo")
      (setq pos (point))
      (proctor-with-helm (aph/helm-semantic-or-imenu nil)
        (should (= 1 (helm-get-candidate-number))))
      (should (= pos (point))))))

(provide 'aph-helm-test)
;;; aph-helm-test.el ends here
