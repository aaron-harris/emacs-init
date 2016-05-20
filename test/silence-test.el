;;; silence-test.el --- Tests for silence.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `silence', `ert', `vizier'

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


;;; Message Suppression
;;;====================
(ert-deftest silence-test-predicate ()
  "Test `silence-message-p'." 
  (let ((silence-list      nil)
        (silence-enabled-p t)
        flag)
    ;; Test empty list
    (should (not (silence-message-p "foo")))
    ;; Test against regexp
    (push "foo.*" silence-list) 
    (should (silence-message-p "foo"))
    (should (silence-message-p "foobar"))
    (should (silence-message-p "barfoo"))
    (should-not (silence-message-p "bar"))
    (should-not (silence-message-p "barrel of monkeys"))
    ;; Test against function 
    (push (lambda (msg) (> (length msg) 8)) silence-list)
    (should (silence-message-p "barrel of monkeys"))
    ;; Respect `silence-enabled-p'
    (let ((silence-enabled-p nil))
      (should-not (silence-message-p "foo")))
    ;; Short-circuit when possible
    (push (lambda (msg) (setq flag t) nil) silence-list)
    (push "foo" silence-list)
    (should (silence-message-p "foo"))
    (should (null flag)) 
    ;; Signal error for invalid entry
    (push 7 silence-list)
    (should-error (silence-message-p "foo")) 
    (setf (car silence-list) (list (make-symbol "foo")))
    (should-error (silence-message-p "foo"))))

(ert-deftest silence-test-advice ()
  "Test the function `silence-advice'."
  (require 'vizier)                     ; For `vizier-with-advice'
  (vizier-with-advice
      ((:genname #'message :around #'silence-advice))
    (let ((silence-list      '("foo"))
          (silence-enabled-p t))
      (should (equal (message "bar") (current-message)))
      (should-not (equal (message "foo") (current-message)))
      (should (equal (message "%s" "foo") "foo"))
      (let ((silence-enabled-p nil))
        (should (equal (message "foo") (current-message))))
      (let ((silence-list nil))
        (should (equal (message "foobar") (current-message)))))))

(ert-deftest silence-test-nil-message ()
  "Test `silence-advice' on nil and empty messages."
  (aph/with-advice ((:genname #'message :around #'silence-advice))
    (let ((silence-list      (list #'null))
          (silence-enabled-p t))
      ;; nil and "" are not silenced
      (message "foo")
      (should (equal (message nil) nil))
      (should (equal (message "")  ""))
      (should (null (current-message)))
      ;; If "" is not a direct argument, it is not treated specially
      (let ((silence-list '("^$")))
        (message "bar")
        (message "%s" "")
        (should (equal (current-message) "bar"))))))

(ert-deftest silence-test-wrapper ()
  "Test `silence' wrapper macro."
  (let ((silence-list      (list "foo"))
        (silence-enabled-p nil))
    (should (equal (message "foo") (current-message)))
    (silence ("bar")
      (message nil)
      (should-not (equal (message "foo") (current-message)))
      (should-not (equal (message "bar") (current-message))))
    (setq silence-enabled-p t)
    (should-not (equal (message "foo") (current-message)))
    (should (equal (message "bar") (current-message)))))


;;; Load Message Suppression
;;;=========================
(ert-deftest silence-test-loading ()
  "Test `silence-loading' macro."
  ;; Mock `load' function for testing.
  ;; This version returns t if a load message would be printed.
  (cl-letf (((symbol-function 'load)
             (lambda (file &optional noerror nomessage nosuffix must-suffix)
               (unless (stringp file) (error "file not a string"))
               (not nomessage))))
    ;; Test mock function.
    (should (load "file"))
    (should-not (load "file" nil t))
    (should-error (load 'file))
    ;; Test macro.
    (silence-loading
      (should-not (load "file")))
    ;; Test cleanup.
    (should (load "file"))))

(provide 'silence-test)
;;; silence-test.el ends here
