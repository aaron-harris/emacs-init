;;; bfw.el --- Tests for bfw.el                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `bfw', `proctor', `seq'

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

(require 'bfw)
(require 'proctor)

(require 'seq)


;;;; Buffers
;;==========
(ert-deftest bfw-test-get-buffers-by-regexp ()
  "Test `bfw-get-buffers-by-regexp'."
  (let ((foo  (get-buffer-create "Foo"))
        (bar  (get-buffer-create "Bar")))
    (unwind-protect
        (let* ((regexp  (regexp-opt (list (buffer-name foo)
                                          (buffer-name bar))))
               (case-fold-search nil)
               (result  (bfw-get-buffers-by-regexp regexp))) 
          (should (= 2 (length result)))
          (should (seq-contains result foo #'eq))
          (should (seq-contains result bar #'eq)))
      (kill-buffer foo)
      (kill-buffer bar))))

(ert-deftest bfw-get-buffer-for-file ()
  "Test `bfw-get-buffer-for-file'."
  (let ((file (expand-file-name "foo" proctor-directory))
        buffer) 
    (proctor-with-file "foo" "Foo"
      (should-not (bfw-get-buffer-for-file file))
      (setq buffer (find-file file))
      (unwind-protect
          (should (eq buffer (bfw-get-buffer-for-file file)))
        (bfw-kill-buffer-if-any buffer)))))

(ert-deftest bfw-test-kill-buffer-if-any ()
  "Test `bfw-kill-buffer-if-any'."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (bfw-kill-buffer-if-any buf :nowarn)
      (should-not (buffer-live-p buf))
      (bfw-kill-buffer-if-any buf :nowarn)))
  (with-temp-buffer
    (let ((name (buffer-name)))
      (bfw-kill-buffer-if-any name :nowarn)
      (should-not (buffer-live-p (get-buffer name)))
      (bfw-kill-buffer-if-any name :nowarn))))

(provide 'bfw)
;;; bfw.el ends here
