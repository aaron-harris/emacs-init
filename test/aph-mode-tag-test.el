;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MODE TAG TESTS
;;;;============================================================================

;; Tests for the module aph-mode-tag.el.

(require 'cl-lib)                       ; For `cl-gensym'


;;; Testing Apparatus
;;;==================
(defmacro aph/with-test-mode-tag (names doc &rest body)
  "Execute BODY in an environment with a temporary mode tag.

More specifically:
- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and create a mode tag (using `aph/mode-tag-create') with this
  name and DOC as its docstring.
- Execute BODY, with the name of the mode tag bound to TAG.  If HOOK
  is supplied, bind to it the name of the hook variable associated
  with the mode tag.
- Make sure the mode tag created does not persist outside this form,
  using `unwind-protect' to ensure the tag is deleted in the event of
  an error or nonlocal exit from BODY.

\(fn (TAG [HOOK]) &rest BODY)"
  (declare (debug ((symbolp &optional symbolp) form body))
           (indent 2))
  (let ((tag   (car names))
        (hook  (or (cadr names) (make-symbol "hook"))))
    `(let* ((,tag   (cl-gensym "tag"))
            (,hook  (aph/symbol-concat ,tag "-tag-hook")))
       (unwind-protect
           (progn (aph/mode-tag-create tag ,doc)
                  ,@body)
         ;; Since tag is uninterned, its symbol properties won't
         ;; persist, but we need to clean up after hook, which was
         ;; created with `defvar'.  It should suffice to unintern it.
         (unintern ,hook)))))

(defmacro aph/with-test-mode (names parent &rest body)
  "Execute BODY in an environment with a temporarily-defined major mode.

More specifically:
- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and create a major mode (using `define-derived-mode') with this
  name.  Evaluate PARENT to get the name of the parent mode.
- Execute BODY, with the name of the new mode bound to MODE.  If HOOK
  is supplied, bind it to the name of the hook variable associated
  with the mode tag.
- Make sure the mode created does not persist outside this form, using
  `unwind-protect' to ensure the tag is deleted in the event of an
  error or nonlocal exit from BODY.

Note that the major mode constructed in this block doesn't actually do
anything (i.e., its body is empty).

\(fn (MODE [HOOK]) PARENT &rest BODY)"
  (declare (debug ((symbolp &optional symbolp) form body))
           (indent 2))
  (let ((mode       (car names))
        (hook       (or (cadr names) (make-symbol "hook"))) 
        (make-mode  (make-symbol "make-mode")))
    `(let* ((,mode       (cl-gensym "mode"))
            (,hook       (aph/symbol-concat ,mode "-hook")) 
            (,make-mode  (lambda (child parent)
                           (eval `(define-derived-mode
                                    ,child ,parent "Lighter")))))
       (unwind-protect
           (progn (funcall ,make-mode ,mode ,parent)
                  ,@body)
         (unintern ,hook)
         (unintern (aph/symbol-concat ,mode "-map"))
         (unintern (aph/symbol-concat ,mode "-syntax-table"))
         (unintern (aph/symbol-concat ,mode "-abbrev-table"))))))


;;; Apparatus Testing
;;;==================
;; Tests to verify that the testing macros just defined function
;; correctly.

(ert-deftest aph/mode-tag-test-with-tag ()
  "Test functionality of the wrapper macro `aph/with-test-mode-tag'.

In particular, we want to make sure of the following points:
- No trace of the mode tag persists outside the form.
- The macro is only as anaphoric as advertised.
- Cleanup occurs even in case of an error.

Note that we do not test that the mode tag actually exists within
the body, deferring this to testing of `aph/mode-tag-create'." 
  ;; Test that body is executed
  (let (foo)
    (should (aph/with-test-mode-tag (tag hook) "doc"
              (setq foo 'bar)
              (= 2 (+ 1 1))))
    (should (eq foo 'bar))
    (should-not (aph/with-test-mode-tag (tag hook) "doc"
                  (= 3 (+ 1 1)))))
  ;; Test that hook binding is correct
  (aph/with-test-mode-tag (tag hook) "doc"
    (should (eq hook (aph/symbol-concat tag "-tag-hook"))))
  ;; Test for unnecessary bindings
  (aph/with-test-mode-tag (tag) "doc"
    (should-error hook :type 'void-variable))
  ;; Test cleanup
  (let (tag-x hook-x)
    (aph/with-test-mode-tag (tag hook) "doc"
      (setq tag-x  tag
            hook-x hook))
    (should-not (intern-soft tag-x))
    (should-not (intern-soft hook-x)))
  ;; Test for cleanup in case of error
  (let (tag-x hook-x)
    (ignore-errors
      (aph/with-test-mode-tag (tag hook) "doc"
        (setq tag-x  tag
              hook-x hook)
        (error "Triggered error")))
    (should-not (intern-soft tag-x))
    (should-not (intern-soft hook-x))))

(ert-deftest aph/mode-tag-test-with-mode ()
  "Test functionality of the wrapper macro `aph/with-test-mode'.

In particular, we want to make sure of the following points:
- No trace of the mode persists outside the form.
- The macro is only as anaphoric as advertised.
- Cleanup occurs even in case of an error." 
  ;; Test that body is executed
  (let (foo)
    (should (aph/with-test-mode (mode hook) 'text-mode
              (setq foo 'bar)
              (= 2 (+ 1 1))))
    (should (eq foo 'bar))
    (should-not (aph/with-test-mode (mode hook) 'text-mode 
                  (= 3 (+ 1 1)))))
  ;; Test that mode and hook exist inside form
  (aph/with-test-mode (mode hook) 'text-mode
    (should (fboundp mode))
    (should (boundp hook)))
  ;; Test parentage
  (aph/with-test-mode (mode1) 'text-mode
    (aph/with-test-mode (mode2) mode1
      (should (eq 'text-mode (get mode1 'derived-mode-parent)))
      (should (eq mode1      (get mode2 'derived-mode-parent)))))
  ;; Test that hook binding is correct
  (aph/with-test-mode (mode hook) 'text-mode
    (should (eq hook (aph/symbol-concat mode "-hook"))))
  ;; Test for unnecessary bindings
  (aph/with-test-mode (mode) 'text-mode
    (should-error hook   :type 'void-variable)
    (should-error keymap :type 'void-variable)
    (should-error syntax :type 'void-variable)
    (should-error abbrev :type 'void-variable))
  ;; Test cleanup
  (let (mode-x hook-x)
    (aph/with-test-mode (mode hook) 'text-mode
      (setq mode-x mode
            hook-x hook)))
  ;; Test for cleanup in case of error
  (let (mode-x hook-x)
    (ignore-errors
      (aph/with-test-mode (mode hook) 'text-mode
        (setq mode-x mode
              hook-x hook)
        (error "Triggered error")))
    (should-not (intern-soft mode-x))
    (should-not (intern-soft hook-x)))) 


;;; Implementation Tests
;;;=====================
;; These tests confirm that these functions are storing things
;; properly in symbol properties; thus, they depend on implementation
;; details.
  
(ert-deftest aph/mode-tag-test-impl-create ()
  "Test implementation of `aph/mode-tag-create'." 
  (aph/with-test-mode-tag (tag hook) "doc"
    ;; Test basic functionality
    (should (get tag 'aph/mode-tag))
    (should (equal "doc" (get tag 'aph/mode-tag-docstring)))
    (should (boundp hook))
    (should (null (eval hook)))
    ;; Test collision with hook variable 
    (setplist tag nil)
    (add-hook hook #'ignore) 
    (should-error (aph/mode-tag-create tag "doc"))
    (should (null (symbol-plist tag)))
    (should (equal (symbol-value hook) (list #'ignore)))))

(ert-deftest aph/mode-tag-test-impl-delete ()
  "Test implementation of `aph/mode-tag-delete'."
  (aph/with-test-mode-tag (tag hook) "doc"
    (aph/mode-tag-delete tag)
    (should (null (get tag 'aph/mode-tag)))
    (should (null (get tag 'aph/mode-tag-docstring)))
    (should-not (boundp hook))
    (should (null (get hook 'variable-documentation)))))

(ert-deftest aph/mode-tag-test-impl-add ()
  "Test implementation of `aph/mode-tag-add'."
  (aph/with-test-mode-tag (tag) "doc"
    (let ((mode (make-symbol "foo-mode")))
      (aph/mode-tag-add mode tag)
      (should (cl-find mode (get tag  'aph/mode-tag-modes)))
      (should (cl-find tag  (get mode 'aph/mode-tag-tags))))))

(ert-deftest aph/mode-tag-test-impl-remove ()
  "Test implementation of `aph/mode-tag-remove'."
  (aph/with-test-mode-tag (tag) "doc"
    (let ((mode (make-symbol "foo-mode")))
      (aph/mode-tag-add mode tag)
      ;; Remove an existing association
      (aph/mode-tag-remove mode tag)
      (should-not (cl-find mode (get tag  'aph/mode-tag-modes)))
      (should-not (cl-find tag  (get mode 'aph/mode-tag-tags))))))


;;; Content Tests
;;;==============
;; Tests in this section verify correct behavior without relying on
;; implementation details.

(ert-deftest aph/mode-tag-test-pred ()
  "Test functionality of `aph/mode-tag-p'."
  (aph/with-test-mode-tag (tag) "doc"
    (should (aph/mode-tag-p tag)))
  (let ((tag (make-symbol "tag")))
    (should-not (aph/mode-tag-p tag))))

(ert-deftest aph/mode-tag-test-create/delete ()
  "Test `aph/mode-tag-create' and `aph/mode-tag-delete'."
  (aph/with-test-mode-tag (tag) "doc"
    (should (aph/mode-tag-p tag))
    (aph/mode-tag-delete tag)
    (should-not (aph/mode-tag-p tag))
    (aph/mode-tag-create tag) 
    (should (aph/mode-tag-p tag))))

(ert-deftest aph/mode-tag-test-add/remove ()
  "Test `aph/mode-tag-add' and `aph/mode-tag-remove'.
Also test `aph/mode-tag-tagged-p'."
  ;; Basic functionality
  (aph/with-test-mode-tag (tag) "doc" 
    (aph/with-test-mode (mode) 'text-mode
      (aph/mode-tag-add mode tag)
      (should (aph/mode-tag-tagged-p mode tag))
      (aph/mode-tag-remove mode tag)
      (should-not (aph/mode-tag-tagged-p mode tag))))
  ;; Test inheritance
  (aph/with-test-mode-tag (tag) "doc"
    (aph/with-test-mode (mode1) 'text-mode
      (aph/with-test-mode (mode2) mode1
        (aph/mode-tag-add mode1 tag)
        (should (aph/mode-tag-tagged-p mode1 tag))
        (should-not (aph/mode-tag-tagged-p mode2 tag))
        (should (aph/mode-tag-tagged-p mode2 tag :inherit))))))

      
(provide 'aph-mode-tag-test)
