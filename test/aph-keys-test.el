;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; APH-KEYS TESTS
;;;;============================================================================

;; Tests for the module `aph-keys'.
(require 'aph-keys)
(require 'aph-ert-test)


;;; Testing Apparatus
;;;==================
(defmacro aph-keys-with-augmented-test-mode (name parent &rest body)
  "As `aph/ert-with-major-mode', but augmented for `aph-keys'.

Instantiate a temporary mode, as in `aph/ert-with-major-mode'.
Additionally, augment the mode with `aph-keys-augment', bind the
augmented keymap to NAME-augmented-map, and ensure that the
variable containing this map does not persist outside of BODY."
  (declare (debug aph/ert-with-major-mode)
           (indent 2))
  (let ((augmap      (aph/symbol-concat name "-augmented-map"))
        (augmap-var  (make-symbol "augmap-var")))
    `(aph/ert-with-major-mode ,name ,parent
       (let ((,augmap-var  (aph-keys-augment-var ,name))
             (,augmap      (aph-keys-augment ,name)))
         (unwind-protect (progn ,@body)
           (unintern ,augmap-var))))))


;;; Apparatus Tests
;;;================
(ert-deftest aph-keys-test-with-augmented-test-mode--body ()
  "Test that `aph-keys-with-augmented-test-mode' executes body."
  (should (aph/ert-macro-executes-body
           'aph-keys-with-augmented-test-mode '(mode 'text-mode))))

(ert-deftest aph-keys-test-with-augmented-test-mode--bindings ()
  "Test bindings of `aph-keys-with-augmented-test-mode'."
  (should (aph/ert-test-mode-wrapper--bindings
           'aph-keys-with-augmented-test-mode '('fundamental-mode)))
  (aph-keys-with-augmented-test-mode mode 'fundamental-mode
    (should (aph-keys-augmented-p mode))
    (should (keymapp mode-augmented-map))))

(ert-deftest aph-keys-test-with-augmented-test-mode--cleanup ()
  "Test cleanup for `aph-keys-with-augmented-test-mode'."
  (should (aph/ert-test-mode-wrapper--cleanup
           'aph-keys-with-augmented-test-mode '('fundamental-mode)))
  (should (aph/ert-macro-does-not-leak
           'aph-keys-with-augmented-test-mode
           '(aph-keys--augment-name mode) '(mode 'fundamental-mode))))


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
  "Test `aph-keys-augment'."
  (let* ((mode    'foo-mode)
         (var     (aph-keys-augment-var mode))
         (keymap  (aph-keys-augment mode)))
    (unwind-protect 
        (progn
          ;; Basic type checks, and relationship verification
          (should (boundp var))
          (should (eq var (aph-keys--augment-name mode)))
          (should (eq keymap (symbol-value var)))
          (should (keymapp keymap))
          ;; Test for idempotence, and preservation of bindings
          (define-key keymap (kbd "a") #'ignore)
          (should (eq keymap (aph-keys-augment mode)))
          (should (eq var (aph-keys-augment-var mode)))
          (should (eq (lookup-key keymap (kbd "a")) #'ignore)))
      (unintern var))))

(ert-deftest aph-keys-test-augmented-p ()
  "Test `aph-keys-augmented-p'."
  (let* ((mode  'foo-mode)
         var)
    (should-not (aph-keys-augmented-p mode))
    (setq var (aph-keys-augment-var mode))
    (unwind-protect
        (should (aph-keys-augmented-p mode))
      (unintern var))))

(ert-deftest aph-keys-test-augment--major ()
  "Test `aph-keys-augment' with major modes."
  (aph-keys-with-augmented-test-mode mode 'fundamental-mode
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
