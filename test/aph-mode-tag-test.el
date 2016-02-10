;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MODE TAG TESTS
;;;;============================================================================

;; Tests for the module aph-mode-tag.el.

(require 'cl-lib)                       ; For `cl-gensym'
(require 'aph-ert)                      ; For `aph/ert-with-test-mode'
(require 'aph-advice)                   ; For `aph/with-advice'


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
           (progn (aph/mode-tag-create ,tag ,doc)
                  ,@body)
         ;; Since tag is uninterned, its symbol properties won't
         ;; persist, but we need to clean up after hook, which was
         ;; created with `defvar'.  It should suffice to unintern it.
         (unintern ,hook)))))


;;; Apparatus Testing: `aph/with-test-mode-tag'
;;;============================================
;; Tests to verify that `aph/with-test-mode-tag', just defined,
;; functions correctly.

(ert-deftest aph/mode-tag-test-with-mode-tag--body ()
  "Test that body of `aph/with-test-mode-tag' is executed."
  (aph/ert-macro-executes-body aph/with-test-mode-tag (tag)))

(ert-deftest aph/mode-tag-test-with-mode-tag--bindings ()
  "Test that `aph/with-test-mode-tag' sets bindings correctly."
  ;; Intentional bindings
  (aph/with-test-mode-tag (foo bar) "doc"
    (should (symbolp foo))
    (should (eq bar (aph/symbol-concat foo "-tag-hook"))))
  ;; No unintentional bindings
  (aph/with-test-mode-tag (foo) "doc"
    (should-error bar :type 'void-variable)))

(ert-deftest aph/mode-tag-test-with-mode-tag--cleanup ()
  "Test that `aph/with-test-mode-tag' cleans up after itself." 
  ;; Normal exit 
  (let (tag-x hook-x)
    (aph/with-test-mode-tag (tag hook) "doc"
      (setq tag-x  tag
            hook-x hook))
    (should-not (intern-soft tag-x))
    (should-not (intern-soft hook-x)))
  ;; Error
  (let (tag-x hook-x)
    (ignore-errors
      (aph/with-test-mode-tag (tag hook) "doc"
        (setq tag-x  tag
              hook-x hook)
        (error "Triggered error")))
    (should-not (intern-soft tag-x))
    (should-not (intern-soft hook-x))))


;;; Content Tests
;;;============== 
(ert-deftest aph/mode-tag-test-pred ()
  "Test functionality of `aph/mode-tag-p'."
  (aph/with-test-mode-tag (tag) "doc"
    (should (aph/mode-tag-p tag)))
  (let ((tag (make-symbol "tag")))
    (should-not (aph/mode-tag-p tag))))

(ert-deftest aph/mode-tag-test-create--init ()
  "Test that `aph/mode-tag-create' initializes tag correctly."
  (aph/with-test-mode-tag (tag hook) "doc"
    (should (equal "doc" (get tag 'aph/mode-tag-docstring)))
    (should (boundp hook))
    (should (null (eval hook)))))

(ert-deftest aph/mode-tag-test-create--redef ()
  "Test handling of existing tags for `aph/mode-tag-create'."
  (aph/with-test-mode-tag (tag hook) "foo"
    (add-hook hook #'ignore)
    (aph/ert-with-test-mode mode 'text-mode
      (aph/mode-tag-add mode tag)
      (aph/mode-tag-create tag "bar")
      (aph/mode-tag-create tag nil)
      (should (aph/mode-tag-p tag))
      (should (equal "bar" (get tag 'aph/mode-tag-docstring)))
      (should (equal (symbol-value hook) (list #'ignore)))
      (should (aph/mode-tag-tagged-p mode tag))))) 

(ert-deftest aph/mode-tag-test-create--out-of-order ()
  "Test that hooks can be added before tag is defined."
  (let ((tag (cl-gensym "tag")))
    (add-hook (aph/symbol-concat tag "-tag-hook") #'ignore)
    (aph/mode-tag-create tag "doc")
    (should (aph/mode-tag-p tag))))

(ert-deftest aph/mode-tag-test-add/remove ()
  "Test association of tags to modes.
This test confirms basic functionality of the functions
`aph/mode-tag-add', `aph/mode-tag-remove', and
`aph/mode-tag-tagged-p'."
  (aph/with-test-mode-tag (tag) "doc" 
    (aph/ert-with-test-mode mode 'text-mode
      (aph/mode-tag-add mode tag)
      (should (aph/mode-tag-tagged-p mode tag))
      (aph/mode-tag-remove mode tag)
      (should-not (aph/mode-tag-tagged-p mode tag)))))

(ert-deftest aph/mode-tag-test-add--new ()
  "Test `aph/mode-tag-add' when tag doesn't yet exist."
  (let ((tag (cl-gensym "tag")))
    (aph/ert-with-test-mode mode 'text-mode
      (unwind-protect
          (progn
            (aph/mode-tag-add mode tag)
            (should (aph/mode-tag-p tag))
            (should (aph/mode-tag-tagged-p mode tag))
            (aph/mode-tag-create tag "doc")
            (should (equal "doc" (get tag 'aph/mode-tag-docstring))))
        (unintern (aph/symbol-concat tag "-tag-hook"))))))

(ert-deftest aph/mode-tag-test-tagged-p--inherit ()
  "Test `aph/mode-tag-tagged-p' on inherited mode tags."
  (aph/with-test-mode-tag (tag) "doc"
    (aph/ert-with-test-mode mode1 'text-mode
      (aph/ert-with-test-mode mode2 mode1
        (aph/mode-tag-add mode1 tag)
        (should (aph/mode-tag-tagged-p mode1 tag))
        (should-not (aph/mode-tag-tagged-p mode2 tag))
        (should (aph/mode-tag-tagged-p mode2 tag :inherit))))))

(ert-deftest aph/mode-tag-test-lists ()
  "Test get-all functions for mode tags.
These are `aph/mode-tag-get-tags-for-mode' and
`aph/mode-tag-get-modes-for-tag'."
  (aph/with-test-mode-tag (tag) "doc" 
    (aph/ert-with-test-mode mode 'text-mode
      (aph/mode-tag-add mode tag)
      (should (cl-find mode (aph/mode-tag-get-modes-for-tag tag)))
      (should (cl-find tag (aph/mode-tag-get-tags-for-mode mode)))
      (aph/mode-tag-remove mode tag)
      (should-not (cl-find mode (aph/mode-tag-get-modes-for-tag tag)))
      (should-not (cl-find tag (aph/mode-tag-get-tags-for-mode mode))))))

(ert-deftest aph/mode-tag-test-hooks--addition ()
  "Test that a tagged mode runs its tags' hooks."
  (let (log)
    (aph/ert-with-test-mode mode 'fundamental-mode
      (aph/with-test-mode-tag (tag1 hook1) "doc"
        (aph/with-test-mode-tag (tag2 hook2) "doc"
          (add-hook hook1 (lambda () (push :hook1 log)))
          (add-hook hook2 (lambda () (push :hook2 log)))
          (aph/mode-tag-add mode tag1)
          (aph/mode-tag-add mode tag2)
          (with-temp-buffer
            (funcall mode)
            (should (cl-find :hook1 log))
            (should (cl-find :hook2 log))))))))

(ert-deftest aph/mode-tag-test-hooks--removal ()
  "Test that a mode doesn't run tag hooks after tag is removed."
  (let (log)
    (aph/ert-with-test-mode mode 'fundamental-mode
      (aph/with-test-mode-tag (tag hook) "doc"
        (add-hook hook (lambda () (push :hook log)))
        (aph/mode-tag-add mode tag)
        (aph/mode-tag-remove mode tag)
        (with-temp-buffer
          (funcall mode)
          (should-not (cl-find :hook log)))))))

(ert-deftest aph/mode-tag-test-hooks--partial-removal ()
  "Test that removing a tag doesn't remove all tag hooks."
  (let (log)
    (aph/ert-with-test-mode mode 'fundamental-mode
      (aph/with-test-mode-tag (tag1 hook1) "doc"
        (aph/with-test-mode-tag (tag2 hook2) "doc"
          (add-hook hook1 (lambda () (push :hook1 log)))
          (add-hook hook2 (lambda () (push :hook2 log)))
          (aph/mode-tag-add mode tag1)
          (aph/mode-tag-add mode tag2)
          (aph/mode-tag-remove mode tag2)
          (with-temp-buffer
            (funcall mode)
            (should (cl-find :hook1 log))
            (should-not (cl-find :hook2 log))))))))

(ert-deftest aph/mode-tag-test-hooks--inheritance ()
  "Test that a mode runs its ancestors' tag hooks (only once)."
  (let (log)
    (aph/ert-with-test-mode mode1 'fundamental-mode
      (aph/ert-with-test-mode mode2 mode1
        (aph/ert-with-test-mode mode3 mode2
          (aph/with-test-mode-tag (tag hook) "doc"
            (add-hook hook (lambda () (push :hook log)))
            (aph/mode-tag-add mode1 tag)
            (aph/mode-tag-add mode2 tag)
            (with-temp-buffer
              (funcall mode3)
              (should (equal log '(:hook))))))))))

      
(provide 'aph-mode-tag-test)
