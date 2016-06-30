;;; vizier-helm-test.el --- Tests for vizier-helm.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `vizier-helm', `ert'

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

(require 'vizier-helm)
(require 'ert)


;;;; Testing Apparatus
;;====================
(defvar vizier-helm-test-source
  (helm-build-dummy-source "Test"
    :action #'ignore)
  "A Helm source for testing purposes.")

(defun vizier-helm-test ()
  "Invoke `helm' and immediately quit.  For testing."
  (add-hook 'helm-update-hook #'keyboard-quit)
  (unwind-protect (helm :sources vizier-helm-test-source)
    (remove-hook 'helm-update-hook #'keyboard-quit)))


;;;; Macros
;;=========
(ert-deftest vizier-helm-test-macro--once ()
  "Test `vizier-with-helm' when `helm' is invoked once."
  (let (canary)
    (vizier-with-helm (vizier-helm-test)
      (should helm-alive-p)
      (should (equal (buffer-name) helm-buffer))
      (setq canary t))
    (should canary)
    (should-not helm-alive-p)))

(ert-deftest vizier-helm-test-macro--none ()
  "Test `vizier-with-helm' when `helm' is not invoked."
  (let (canary)
    (vizier-with-helm (ignore)
      (setq canary t))
    (should-not canary)))

(ert-deftest vizier-helm-test-macro--twice ()
  "Test `vizier-with-helm' when `helm' is invoked twice."
  (let ((canary 0))
    (vizier-with-helm
        (progn (vizier-helm-test)
               (vizier-helm-test))
      (setq canary (1+ canary)))
    (should (= canary 1))))

(provide 'vizier-helm-test)
;;; vizier-helm-test.el ends here
