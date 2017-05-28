;;; proctor-helm.el --- ERT support for `helm'       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `vizier-helm'

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

;;; Commentary:

;; This module contains macros and functions designed for testing
;; `helm' commands.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'vizier-helm)


;;;; Fixtures
;;===========
(defmacro proctor-with-helm (trigger &rest body)
  "As `vizier-with-helm', but quit `helm' after BODY.

Also, override `helm--current-buffer' so that the correct buffer
is used as `helm-current-buffer' when invoked from an `ert'
test."
  (declare (indent 1)
           (debug  t))
  (let ((buf (make-symbol "buf")))
    `(cl-letf* ((,buf (current-buffer))
                ((symbol-function 'helm--current-buffer)
                 (lambda () ,buf)))
       (vizier-with-helm ,trigger
         ,@body
         (keyboard-quit)))))


;;;; Test Source
;;==============
(defvar proctor-helm-empty-source
  (helm-build-dummy-source "Test"
    :action #'ignore)
  "A `helm' source for testing purposes.")

(defun proctor-helm-empty ()
  "Invoke `helm' with no candidates, for testing purposes."
  (helm :sources proctor-helm-empty-source))

(provide 'proctor-helm)
;;; proctor-helm.el ends here
