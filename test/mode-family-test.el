;;; mode-family-test.el --- Tests for mode-family.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Required features: `mode-family', `proctor'

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

(require 'mode-family)
(require 'proctor)

(eval-when-compile (require 'dash))     ; For `->>', `-lambda'


;;; Testing Apparatus
;;;==================
(defmacro mode-family-test (&rest body)
  "Execute BODY in an \"safe\" environment w.r.t. mode families.

Inside BODY, none of the mode families that normally exist will
be in effect, and no changes made to mode families inside BODY
will persist once BODY exits.  This includes changes to mode
family hooks."
  (declare (debug body)) 
  `(let ((saved-hook-alist
          (mapcar (lambda (family)
                    (let ((family-hook (mode-family--hook family)))
                      (prog1 `(,family . ,(->> family-hook
                                               symbol-value
                                               copy-list))
                        (set family-hook nil))))
                  mode-family--list))
         mode-family--list
         mode-family--alist)
     (unwind-protect (progn ,@body)
       (dolist (family mode-family--list)
         (unless (memq family saved-hook-alist)          
           (makunbound (mode-family--hook family))))
       (mapc (-lambda ((family . hooks)) 
               (set (mode-family--hook family) hooks))
             saved-hook-alist))))


;;; Content Tests
;;;============== 
(ert-deftest mode-family-test-create--basic ()
  "Test basic functionality of `mode-family-create'."
  (mode-family-test
   (should-not (mode-family-p 'foo))
   (mode-family-create 'foo)
   (should (mode-family-p 'foo))
   (should (boundp 'foo-family-hook))
   (should (null foo-family-hook))))

(ert-deftest mode-family-test-create--redef ()
  "Test handling of existing families for `mode-family-create'. 
Also test adding hooks out-of-order."
  (mode-family-test
   (add-hook 'foo-family-hook #'ignore)
   (mode-family-create 'foo)
   (proctor-with-major-mode mode 'text-mode
     (mode-family-add mode 'foo)
     (mode-family-create 'foo)
     (should (mode-family-p 'foo))
     (should (equal foo-family-hook (list #'ignore)))
     (should (mode-family-member-p mode 'foo)))))

(ert-deftest mode-family-test-add/remove ()
  "Test association of families to modes.

This test confirms basic functionality of the functions
`mode-family-add', `mode-family-remove', and
`mode-family-member-p'." 
  (mode-family-test
   (mode-family-create 'foo)
    (proctor-with-major-mode mode 'text-mode
      (mode-family-add mode 'foo)
      (should (mode-family-member-p mode 'foo))
      (mode-family-remove mode 'foo)
      (should-not (mode-family-member-p mode 'foo)))))

(ert-deftest mode-family-test-add--new ()
  "Test `mode-family-add' when family doesn't yet exist."
  (mode-family-test
   (proctor-with-major-mode mode 'text-mode
     (mode-family-add mode 'foo)
     (should (mode-family-p 'foo))
     (should (mode-family-member-p mode 'foo)))))

(ert-deftest mode-family-test-member-p--inherit ()
  "Test `mode-family-member-p' on inherited mode families."
  (mode-family-test
   (mode-family-create 'foo)
   (proctor-with-major-mode mode1 'text-mode
     (proctor-with-major-mode mode2 mode1
       (mode-family-add mode1 'foo)
       (should (mode-family-member-p mode1 'foo))
       (should-not (mode-family-member-p mode2 'foo))
       (should (mode-family-member-p mode2 'foo :inherit))))))

(ert-deftest mode-family-test-lists ()
  "Test get-all functions for mode families.

These functions are `mode-family-list-families' and
`mode-family-list-members'."
  (mode-family-test
   (mode-family-create 'foo)
    (proctor-with-major-mode mode 'text-mode
      (mode-family-add mode 'foo)
      (should (memq mode (mode-family-list-members 'foo)))
      (should (memq 'foo (mode-family-list-families mode)))
      (mode-family-remove mode 'foo)
      (should-not (memq mode (mode-family-list-members 'foo)))
      (should-not (memq 'foo (mode-family-list-families mode))))))

(ert-deftest mode-family-test-hooks--addition ()
  "Test that a mode runs its families' hooks."
  (mode-family-test
   (let (log)
     (proctor-with-major-mode mode 'fundamental-mode
       (mode-family-create 'foo)
       (mode-family-create 'bar)
       (add-hook 'foo-family-hook (lambda () (push :foo log)))
       (add-hook 'bar-family-hook (lambda () (push :bar log)))
       (mode-family-add mode 'foo)
       (mode-family-add mode 'bar)
       (with-temp-buffer
         (funcall mode)
         (should (memq :foo log))
         (should (memq :bar log)))))))

(ert-deftest mode-family-test-hooks--removal ()
  "Test that a mode doesn't run family hooks after removal."
  (mode-family-test
   (let (log)
     (proctor-with-major-mode mode 'fundamental-mode
       (mode-family-create 'foo)
       (add-hook 'foo-family-hook (lambda () (push :hook log)))
       (mode-family-add mode 'foo)
       (mode-family-remove mode 'foo)
       (with-temp-buffer
         (funcall mode)
         (should-not (memq :hook log)))))))

(ert-deftest mode-family-test-hooks--partial-removal ()
  "Test that removing a family doesn't remove all family hooks."
  (mode-family-test 
   (let (log)
     (proctor-with-major-mode mode 'fundamental-mode
       (mode-family-create 'foo)
       (mode-family-create 'bar)
       (add-hook 'foo-family-hook (lambda () (push :foo log)))
       (add-hook 'bar-family-hook (lambda () (push :bar log)))
       (mode-family-add mode 'foo)
       (mode-family-add mode 'bar)
       (mode-family-remove mode 'bar)
       (with-temp-buffer
         (funcall mode)
         (should (memq :foo log))
         (should-not (memq :bar log)))))))

(ert-deftest mode-family-test-hooks--inheritance ()
  "Test that a mode runs its ancestors' family hooks (only once)."
  (mode-family-test 
   (mode-family-create 'foo) 
   (let (log)
     (proctor-with-major-mode     mode1  'fundamental-mode
       (proctor-with-major-mode   mode2  mode1
         (proctor-with-major-mode mode3  mode2
           (add-hook 'foo-family-hook (lambda () (push :hook log)))
           (mode-family-add mode1 'foo)
           (mode-family-add mode2 'foo)
           (with-temp-buffer
             (funcall mode3)
             (should (equal log '(:hook))))))))))


(provide 'mode-family-test)
;;; mode-family-test.el ends here
