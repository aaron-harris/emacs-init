;;; eimp-ephemeral-test.el --- Tests for eimp-ephemeral.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `eimp-ephemeral', `ert-x'

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

(require 'eimp-ephemeral)
(require 'ert-x)


;;;; Test Apparatus
;;=================
(defvar eimp-ephemeral-test-image "eimp-ephemeral-test-image.png"
  "The path to the standard test image.")

(defmacro eimp-ephemeral-test-fixture (&rest body)
  "Evaluate BODY in a buffer containing a standard test image.

The buffer used has the following properties:

- It contains the image found in `eimp-ephemeral-test-image' (a
  32 x 32 solid blue square).

- It is in `image-mode'.

- It is not considered modified (per `buffer-modified-p').

- It has been created with `ert-with-test-buffer', so that it
  will persist only in the event of a failed test."
  (declare (indent 0)
           (debug  t))
  `(ert-with-test-buffer ()
    (insert-file-contents-literally eimp-ephemeral-test-image)
    (image-mode)
    (set-buffer-modified-p nil)
    ,@body))

(defun eimp-ephemeral-test-transform (transform &rest args)
  "Apply TRANSFORM to ARGS and wait for completion.

Here TRANSFORM is an EIMP transform."
  (apply transform args)
  (while (get-buffer-process (current-buffer))
    (sleep-for 1)))


;;;; Tests
;;========
(ert-deftest eimp-ephemeral-test-control ()
  "Test that EIMP itself is working as expected.
This doubles as a test for `eimp-ephemeral-test-fixture'."
  (eimp-ephemeral-test-fixture
    (should-not (buffer-modified-p))
    (eimp-ephemeral-test-transform #'eimp-increase-image-size 150)
    (should (equal '(48 . 48) (image-size (eimp-get-image) :pixels)))
    (should (buffer-modified-p))))

(ert-deftest eimp-ephemeral-test-apply ()
  "Test `eimp-ephemeral-apply'."
  (eimp-ephemeral-test-fixture
    (eimp-ephemeral-test-transform #'eimp-ephemeral-apply
                                   #'eimp-increase-image-size 150)
    (should (equal '(48 . 48) (image-size (eimp-get-image) :pixels)))
    (should-not (buffer-modified-p))))

(provide 'eimp-ephemeral-test)
;;; eimp-ephemeral-test.el ends here
