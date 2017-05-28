;;; proctor-helm-test.el --- Tests for proctor-helm.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `proctor-helm', `ert-x'

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

(require 'proctor-helm)
(require 'ert-x)


;;;; Fixtures
;;===========
(ert-deftest proctor-helm-test-macro--once ()
  "Test `proctor-with-helm' when `helm' is invoked once."
  (ert-with-test-buffer ()
    (let ((buf  (current-buffer))
          canary)
      (proctor-with-helm (proctor-helm-empty)
        (should helm-alive-p) 
        (should (equal (buffer-name) helm-buffer))
        (should (eq buf helm-current-buffer))
        (setq canary t))
      (should canary)
      (should-not helm-alive-p))))

(ert-deftest proctor-helm-test-macro--none ()
  "Test `proctor-with-helm' when `helm' is not invoked."
  (let (canary)
    (proctor-with-helm (ignore)
      (setq canary t))
    (should-not canary)))

(ert-deftest proctor-helm-test-macro--twice ()
  "Test `proctor-with-helm' when `helm' is invoked twice."
  (let ((canary 0))
    (proctor-with-helm
        (progn (proctor-helm-empty)
               (proctor-helm-empty))
      (setq canary (1+ canary)))
    (should (= canary 2))))


;;;; Test Source
;;==============
(ert-deftest proctor-helm-test-empty ()
  "Test `proctor-helm-empty'."
  (proctor-with-helm (proctor-helm-empty)
    (should (= 0 (helm-get-candidate-number))))) 

(provide 'proctor-helm-test)
;;; proctor-helm-test.el ends here
