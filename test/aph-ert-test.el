;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ERT TESTS
;;;;============================================================================

;; Tests for the module `aph-ert'.
(require 'aph-ert)


;;; Macro Testing Apparatus Tests
;;;==============================
(ert-deftest aph/ert-test-macro-executes-body ()
  "Test `aph/ert-macro-executes-body'." 
  (should (aph/ert-macro-executes-body 'with-temp-buffer))
  (should (aph/ert-macro-executes-body 'let '((canary))))
  (should-error (aph/ert-macro-executes-body 'ignore)))

(ert-deftest aph/ert-test-macro-does-not-leak ()
  "Test `aph/ert-macro-does-not-leak-p'."
  (should (aph/ert-macro-does-not-leak 'let 'var-x '((var-x))))
  (should-error (aph/ert-macro-does-not-leak-p
                 'let ''emacs-version '((var-x)))))


;;; Buffer Handling Apparatus Tests
;;;================================
(ert-deftest aph/ert-test-with-buffer ()
  "Test `aph/ert-with-buffer'."
  (should (aph/ert-macro-executes-body 'aph/ert-with-buffer
                                       '('text-mode "Foo")))
  (dolist (text '("Foo" "\nFoo"))
    (aph/ert-with-buffer 'text-mode text
      (should (eq major-mode 'text-mode))
      (should (looking-at-p "Foo")))))

(ert-deftest aph/ert-test-protecting-buffer ()
  "Test `aph/ert-protecting-buffer'." 
  (with-current-buffer (generate-new-buffer "Foo")
    (let ((name (buffer-name)))
      (should (aph/ert-macro-executes-body 'aph/ert-protecting-buffer `(,name)))
      (should (equal name (buffer-name)))
      (insert "Foo")
      (aph/ert-protecting-buffer name
        (with-current-buffer (generate-new-buffer name)
          (should (equal name (buffer-name)))
          (should (equal "" (buffer-string)))))
      (should (equal "Foo" (buffer-string))))))


;;; Mode Testing Apparatus Tests: Parametrizations
;;;===============================================
(defun aph/ert-test-mode-wrapper--bindings (macro &optional other-args)
  "Test that MACRO binds variables per `aph/ert--with-test-mode'.

Here MACRO should be a symbol naming a macro with the same
general contract as `aph/ert--with-test-mode'.  That is:
- MACRO should have a signature like
  (MACRO NAME [OTHER-ARGS ...] &rest BODY).
- Inside BODY, the variables NAME, NAME-hook, and NAME-map should be
  respectively bound to a mode of some kind, that mode's hook
  variable, and the mode's keymap.
- The above bindings should not persist outside of BODY.

Run a test confirming that the bindings are all appropriately
made inside BODY.  If they are, return t.  Otherwise, signal an
error using `should'."
  (let ((test
         `(,macro mode ,@other-args
                  (should (fboundp mode))
                  (should (eq mode-hook
                              (aph/symbol-concat mode "-hook")))
                  (should (boundp mode-hook))
                  (should (eq mode-map
                              (symbol-value (aph/symbol-concat mode "-map"))))
                  (should (keymapp mode-map)))))
    (eval test)))

(defun aph/ert-test-mode-wrapper--cleanup (macro &optional other-args)
  "Test that MACRO cleans up after itself.

Here MACRO should be a symbol naming a macro with the same
general contract as `aph/ert--with-test-mode'.  That is: - MACRO
should have a signature like
  (MACRO NAME [OTHER-ARGS ...] &rest BODY).
- Inside BODY, the variables NAME, NAME-hook, and NAME-map should be
  respectively bound to a mode of some kind, that mode's hook
  variable, and the mode's keymap.
- The above bindings should not persist outside of BODY.

Run a test confirming that the bindings do not persist after BODY
exits.  If they do not, return t.  Otherwise, signal an error
using `should'."
  (let ((args      (cons 'mode other-args))
        (var-forms '(mode
                     mode-hook
                     (aph/symbol-concat mode "-map"))))
    (dolist (v var-forms t)
      (should (aph/ert-macro-does-not-leak macro v args)))))


;;; Mode Testing Apparatus Tests: `aph/ert-with-major-mode'
;;;========================================================
(ert-deftest aph/ert-test-with-major-mode--body ()
  "Test that body of `aph/ert-with-major-mode' is executed."
  (should (aph/ert-macro-executes-body
           'aph/ert-with-major-mode '(mode 'fundamental-mode))))

(ert-deftest aph/ert-test-with-major-mode--bindings ()
  "Test that `aph/ert-with-major-mode' sets bindings correctly."
  (should (aph/ert-test-mode-wrapper--bindings
           'aph/ert-with-major-mode '('fundamental-mode))))

(ert-deftest aph/ert-test-with-major-mode--cleanup ()
  "Test that `aph/ert-with-major-mode' cleans up after itself."
  (should (aph/ert-test-mode-wrapper--cleanup
           'aph/ert-with-major-mode '('fundamental-mode))))

(ert-deftest aph/ert-test-with-major-mode--nesting ()
  "Test that `aph/ert-with-major-mode' nests properly."
  (aph/ert-with-major-mode mode1 'text-mode
    (aph/ert-with-major-mode mode2 mode1
      (should (eq 'text-mode (get mode1 'derived-mode-parent)))
      (should (eq mode1      (get mode2 'derived-mode-parent))))))


;;; Mode Testing Apparatus Tests: `aph/ert-with-minor-mode'
;;;========================================================
(ert-deftest aph/ert-test-with-minor-mode--body ()
  "Test that body of `aph/ert-with-minor-mode' is executed."
  (should (aph/ert-macro-executes-body
           'aph/ert-with-minor-mode '(mode))))

(ert-deftest aph/ert-test-with-minor-mode--bindings ()
  "Test that `aph/ert-with-minor-mode' sets bindings correctly."
  (should (aph/ert-test-mode-wrapper--bindings
           'aph/ert-with-minor-mode))
  ;; Check binding for minor mode control variable:
  (aph/ert-with-minor-mode mode
      (should (boundp mode))))

(ert-deftest aph/ert-test-with-minor-mode--cleanup ()
  "Test that `aph/ert-with-minor-mode' cleans up after itself."
  (should (aph/ert-test-mode-wrapper--cleanup
           'aph/ert-with-minor-mode))
  ;; Clean up `minor-mode-map-alist', too.
  (let (mode-x)
    (aph/ert-with-minor-mode mode
      (setq mode-x mode)
      (should (assoc mode-x minor-mode-map-alist)))
    (should-not (assoc mode-x minor-mode-map-alist))))


(provide 'aph-ert-test)
