;;; mode-family-test.el --- Tests for mode-family.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Required features: `mode-family', `aph-ert', `cl-lib'

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
(require 'aph-ert)                      ; For `aph/ert-with-test-mode'

(require 'cl-lib)                       ; For `cl-gensym'


;;; Testing Apparatus
;;;==================
(defmacro with-test-mode-family (name doc &rest body)
  "Execute BODY in an environment with a temporary mode family.

More specifically:

- Use `cl-gensym' to construct a name guaranteed not to already
  be in use, and create a mode family (using `mode-family-create')
  with this name and DOC as its docstring.

- Execute BODY, with the name of the mode family bound to NAME
  and the name of the associated hook variable bound to the
  symbol NAME-hook.

- Make sure the mode family created does not persist outside this
  form, using `unwind-protect' to ensure the family is deleted in
  the event of an error or nonlocal exit from BODY."
  (declare (debug ((symbolp &optional symbolp) form body))
           (indent 2))
  (let ((hook  (aph/symbol-concat name "-hook"))) 
    `(let* ((,name  (cl-gensym "family"))
            (,hook  (mode-family--hook ,name)))
       (unwind-protect
           (progn (mode-family-create ,name ,doc)
                  ,@body)
         ;; Since family is uninterned, its symbol properties won't
         ;; persist, but we need to clean up after hook, which was
         ;; created with `defvar'.  It should suffice to unintern it.
         (unintern ,hook)))))


;;; Apparatus Testing: `with-test-mode-family'
;;;===========================================
(ert-deftest with-test-mode-family--body ()
  "Test that body of `with-test-mode-family' is executed."
  (aph/ert-macro-executes-body 'with-test-mode-family '(family "doc")))

(ert-deftest with-test-mode-family--bindings ()
  "Test that `with-test-mode-family' sets bindings correctly."
  ;; Intentional bindings
  (with-test-mode-family family "doc"
    (should (symbolp family))
    (should (symbolp family-hook))
    (should (boundp family-hook))
    (should (eq family-hook (mode-family--hook family)))))

(ert-deftest with-test-mode-family--cleanup ()
  "Test that `with-test-mode-family' cleans up after itself." 
  ;; Normal exit 
  (let (family-x hook-x)
    (with-test-mode-family family "doc"
      (setq family-x  family
            hook-x    family-hook))
    (should-not (intern-soft family-x))
    (should-not (intern-soft hook-x)))
  ;; Error
  (let (family-x hook-x)
    (ignore-errors
      (with-test-mode-family family "doc"
        (setq family-x  family
              hook-x    family-hook)
        (error "Triggered error")))
    (should-not (intern-soft family-x))
    (should-not (intern-soft hook-x))))


;;; Content Tests
;;;============== 
(ert-deftest mode-family-test-pred ()
  "Test functionality of `mode-family-p'."
  (with-test-mode-family family "doc"
    (should (mode-family-p family)))
  (let ((family (make-symbol "family")))
    (should-not (mode-family-p family))))

(ert-deftest mode-family-test-create--init ()
  "Test that `mode-family-create' initializes family correctly."
  (with-test-mode-family family "doc"
    (should (equal "doc" (get family 'mode-family-docstring)))
    (should (boundp family-hook))
    (should (null (symbol-value family-hook)))))

(ert-deftest mode-family-test-create--redef ()
  "Test handling of existing families for `mode-family-create'."
  (with-test-mode-family family "foo"
    (add-hook family-hook #'ignore)
    (aph/ert-with-major-mode mode 'text-mode
      (mode-family-add mode family)
      (mode-family-create family "bar")
      (mode-family-create family nil)
      (should (mode-family-p family))
      (should (equal "bar" (get family 'mode-family-docstring)))
      (should (equal (symbol-value family-hook) (list #'ignore)))
      (should (mode-family-member-p mode family))))) 

(ert-deftest mode-family-test-create--out-of-order ()
  "Test that hooks can be added before family is defined."
  (let ((family (cl-gensym "family")))
    (add-hook (mode-family--hook family) #'ignore)
    (mode-family-create family "doc")
    (should (mode-family-p family))))

(ert-deftest mode-family-test-add/remove ()
  "Test association of familiess to modes.

This test confirms basic functionality of the functions
`mode-family-add', `mode-family-remove', and
`mode-family-member-p'."
  (with-test-mode-family family "doc" 
    (aph/ert-with-major-mode mode 'text-mode
      (mode-family-add mode family)
      (should (mode-family-member-p mode family))
      (mode-family-remove mode family)
      (should-not (mode-family-member-p mode family)))))

(ert-deftest mode-family-test-add--new ()
  "Test `mode-family-add' when family doesn't yet exist."
  (let ((family (cl-gensym "family")))
    (aph/ert-with-major-mode mode 'text-mode
      (unwind-protect
          (progn
            (mode-family-add mode family)
            (should (mode-family-p family))
            (should (mode-family-member-p mode family))
            (mode-family-create family "doc")
            (should (equal "doc" (get family 'mode-family-docstring))))
        (unintern (mode-family--hook family))))))

(ert-deftest mode-family-test-member-p--inherit ()
  "Test `mode-family-member-p' on inherited mode families."
  (with-test-mode-family family "doc"
    (aph/ert-with-major-mode mode1 'text-mode
      (aph/ert-with-major-mode mode2 mode1
        (mode-family-add mode1 family)
        (should (mode-family-member-p mode1 family))
        (should-not (mode-family-member-p mode2 family))
        (should (mode-family-member-p mode2 family :inherit))))))

(ert-deftest mode-family-test-lists ()
  "Test get-all functions for mode families.

These functions are `mode-family-list-families' and
`mode-family-list-members'."
  (with-test-mode-family family "doc" 
    (aph/ert-with-major-mode mode 'text-mode
      (mode-family-add mode family)
      (should (memq mode (mode-family-list-members family)))
      (should (memq family (mode-family-list-families mode)))
      (mode-family-remove mode family)
      (should-not (memq mode (mode-family-list-members family)))
      (should-not (memq family (mode-family-list-families mode))))))

(ert-deftest mode-family-test-hooks--addition ()
  "Test that a mode runs its families' hooks."
  (let (log)
    (aph/ert-with-major-mode mode 'fundamental-mode
      (with-test-mode-family family-1 "doc"
        (with-test-mode-family family-2 "doc"
          (add-hook family-1-hook (lambda () (push :hook1 log)))
          (add-hook family-2-hook (lambda () (push :hook2 log)))
          (mode-family-add mode family-1)
          (mode-family-add mode family-2)
          (with-temp-buffer
            (funcall mode)
            (should (memq :hook1 log))
            (should (memq :hook2 log))))))))

(ert-deftest mode-family-test-hooks--removal ()
  "Test that a mode doesn't run family hooks after removal."
  (let (log)
    (aph/ert-with-major-mode mode 'fundamental-mode
      (with-test-mode-family family "doc"
        (add-hook family-hook (lambda () (push :hook log)))
        (mode-family-add mode family)
        (mode-family-remove mode family)
        (with-temp-buffer
          (funcall mode)
          (should-not (memq :hook log)))))))

(ert-deftest mode-family-test-hooks--partial-removal ()
  "Test that removing a family doesn't remove all family hooks."
  (let (log)
    (aph/ert-with-major-mode mode 'fundamental-mode
      (with-test-mode-family family-1 "doc"
        (with-test-mode-family family-2 "doc"
          (add-hook family-1-hook (lambda () (push :hook1 log)))
          (add-hook family-2-hook (lambda () (push :hook2 log)))
          (mode-family-add mode family-1)
          (mode-family-add mode family-2)
          (mode-family-remove mode family-2)
          (with-temp-buffer
            (funcall mode)
            (should (memq :hook1 log))
            (should-not (memq :hook2 log))))))))

(ert-deftest mode-family-test-hooks--inheritance ()
  "Test that a mode runs its ancestors' family hooks (only once)."
  (let (log)
    (aph/ert-with-major-mode mode1 'fundamental-mode
      (aph/ert-with-major-mode mode2 mode1
        (aph/ert-with-major-mode mode3 mode2
          (with-test-mode-family family "doc"
            (add-hook family-hook (lambda () (push :hook log)))
            (mode-family-add mode1 family)
            (mode-family-add mode2 family)
            (with-temp-buffer
              (funcall mode3)
              (should (equal log '(:hook))))))))))


(provide 'mode-family-test)
;;; mode-family-test.el ends here
