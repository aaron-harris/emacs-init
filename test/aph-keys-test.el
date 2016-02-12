;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; APH-KEYS TESTS
;;;;============================================================================

;; Tests for the module `aph-keys'.
(require 'aph-keys)
(require 'aph-ert-test)


;;; Testing Apparatus
;;;==================
(defmacro aph-keys-with-augmented-mode (name parent-or-type &rest body)
  "Execute BODY with temporarily-defined augmented mode.

The temporary mode is instantiated either with
`aph/ert-with-major-mode' or `aph/ert-with-minor-mode'.  If
PARENT-OR-TYPE is the special keyword :minor, this is a minor
mode; otherwise, it is a major mode, and PARENT-OR-TYPE is the
name of its parent mode.

Also augment this mode with `aph/keys-augment', bind the
augmented keymap to NAME-augmented-map, and ensure that the
variable containing this map does not persist when BODY exits."
  (declare (debug aph/ert-with-major-mode)
           (indent 2))
  (let ((macro-form  (if (eq parent-or-type :minor)
                         `(aph/ert-with-minor-mode ,name)
                       `(aph/ert-with-major-mode ,name ,parent-or-type)))
        (augmap      (aph/symbol-concat name "-augmented-map"))
        (augmap-var  (make-symbol "augmap-var")))
    `(,@macro-form
       (let ((,augmap-var  (aph-keys-augment-var ,name))
             (,augmap      (aph-keys-augment ,name)))
         (unwind-protect (progn ,@body)
           (unintern ,augmap-var))))))


;;; Apparatus Tests
;;;================
(ert-deftest aph-keys-test-with-augmented-mode--body ()
  "Test that `aph-keys-with-augmented-mode' executes body."
  (dolist (param '('fundamental-mode :minor))
    (should (aph/ert-macro-executes-body
             'aph-keys-with-augmented-mode
             `(mode ,param)))))

(ert-deftest aph-keys-test-with-augmented-mode--bindings ()
  "Test bindings of `aph-keys-with-augmented-mode'."
  (dolist (param '('fundamental-mode :minor))
    (should (aph/ert-test-mode-wrapper--bindings
             'aph-keys-with-augmented-mode
             `(,param)))
    (aph-keys-with-augmented-mode mode param
      (should (aph-keys-augmented-p mode))
      (should (keymapp mode-augmented-map)))))

(ert-deftest aph-keys-test-with-augmented-mode--cleanup ()
  "Test cleanup for `aph-keys-with-augmented-mode'."
  (dolist (param '('fundamental-mode :minor))
    (should (aph/ert-test-mode-wrapper--cleanup
             'aph-keys-with-augmented-mode
             `(,param)))
    (should (aph/ert-macro-does-not-leak
             'aph-keys-with-augmented-mode
             ''mode-augmented-map
             `(mode ,param)))))


;;; Tests for `emulation-mode-map-alists' setup
;;;============================================
(ert-deftest aph-keys-test-emma-setup ()
  "Test `emulation-mode-map-alists' setup for `aph-keys-mode'."
  (require 'dash)
  ;; Both of the symbols `aph-keys-augment-map-alist' and
  ;; `aph-keys-local-map-alist' should appear in
  ;; `emulation-mode-map-alists', and they should occur in that order.
  (should (member 'aph-keys-local-map-alist
                  (should (member 'aph-keys-augment-map-alist
                                  emulation-mode-map-alists)))))


;;; Augmented Keymap Tests
;;;================================
(ert-deftest aph-keys-test-augment ()
  "Test `aph-keys-augment' and `aph-keys-augment-var'."
  (require 'aph-dash)                   ; For `aph/eq'
  (dolist (param '('fundamental-mode :minor))
    (aph-keys-with-augmented-mode mode param
      (should (aph/eq mode-augmented-map
                      (aph-keys-augment mode)
                      (symbol-value (aph-keys-augment-var mode))))
      (should (equal (aph-keys-augment-var mode)
                     (aph-keys--augment-name mode)))
      (define-key mode-augmented-map (kbd "a") #'ignore)
      (should (eq (lookup-key (aph-keys-augment mode) (kbd "a"))
                  #'ignore)))))

(ert-deftest aph-keys-test-augmented-p ()
  "Test `aph-keys-augmented-p'." 
  (let* ((mode  'foo-mode)
         var)
    (should-not (aph-keys-augmented-p mode))
    (setq var (aph-keys-augment-var mode))
    (unwind-protect
        (should (aph-keys-augmented-p mode))
      (unintern var))))

(ert-deftest aph-keys-test-major-mode-bindings ()
  "Test major bindings in `aph-keys-mode'." 
  (aph-keys-with-augmented-mode mode 'fundamental-mode
    (let ((aph-keys-mode nil))
      (with-temp-buffer
        (funcall mode)
        (should (eq (key-binding "a") #'self-insert-command))
        (define-key mode-map "a" #'move-beginning-of-line)
        (define-key (aph-keys-augment mode) "a" #'ignore)
        (should (eq (key-binding "a") #'move-beginning-of-line))
        (aph-keys-mode 1)
        (should (eq (key-binding "a") #'ignore))
        (text-mode)
        (should (eq (key-binding "a") #'self-insert-command))))))


(provide 'aph-keys-test)
