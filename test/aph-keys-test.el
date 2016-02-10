;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; APH-KEYS TESTS
;;;;============================================================================

;; Tests for the module `aph-keys'.
(require 'aph-keys)


;;; Testing Apparatus
;;;==================
(defmacro aph-keys-with-augmented-test-mode (name parent &rest body)
  "As `aph/ert-with-test-mode', but augmented for `aph-keys'.

Instantiate a temporary mode, as in `aph/ert-with-test-mode'.
Additionally, augment the mode with `aph-keys-augment', bind the
augmented keymap to NAME-augmented-map, and ensure that the
variable containing this map does not persist outside of BODY."
  (declare (debug aph/ert-with-test-mode)
           (indent 2))
  (let ((augmap      (aph/symbol-concat name "-augmented-map"))
        (augmap-var  (make-symbol "augmap-var")))
    `(aph/ert-with-test-mode ,name ,parent
       (let ((,augmap-var  (aph-keys-augment-var ,name))
             (,augmap      (aph-keys-augment ,name)))
         (unwind-protect (progn ,@body)
           (unintern ,augmap-var))))))


;;; Apparatus Tests
;;;================
(ert-deftest aph-keys-test-with-augmented-test-mode--body ()
  "Test that `aph-keys-with-augmented-test-mode' executes body."
  (aph/ert-macro-executes-body aph-keys-with-augmented-test-mode
                               mode 'text-mode))

(ert-deftest aph-keys-test-with-augmented-test-mode--bindings ()
  "Test bindings of `aph-keys-with-augmented-test-mode'."
  ;; Intentional bindings
  (aph-keys-with-augmented-test-mode mode 'text-mode
    ;; Augmented bindings
    (should (aph-keys-augmented-p mode))
    (should (keymapp mode-augmented-map))
    ;; Bindings from `aph/ert-with-test-mode'
    (should (fboundp mode))
    (should (eq mode-hook (aph/symbol-concat mode "-hook")))
    (should (boundp mode-hook))
    (should (eq mode-map (symbol-value (aph/symbol-concat mode "-map"))))
    (should (keymapp mode-map)))
  ;; No unintentional bindings
  (aph-keys-with-augmented-test-mode mode 'text-mode
    (should-error augmap :type 'void-variable)))

(ert-deftest aph-keys-test-with-augmented-test-mode--cleanup ()
  "Test cleanup for `aph-keys-with-augmented-test-mode'."
  ;; Normal exit
  (let (mode-x hook-x keymap-x augmap-x)
    (aph-keys-with-augmented-test-mode mode 'text-mode
      (setq mode-x     mode
            hook-x     mode-hook
            keymap-x   (aph/symbol-concat mode "-map")
            augmap-x   (aph-keys--augment-name mode)))
    (should-not (intern-soft mode-x))
    (should-not (intern-soft hook-x))
    (should-not (intern-soft keymap-x))
    (should-not (intern-soft augmap-x)))
  ;; Error
  (let (mode-x hook-x)
    (ignore-errors
      (aph-keys-with-augmented-test-mode mode 'text-mode
        (setq mode-x   mode
            hook-x     mode-hook
            keymap-x   (aph/symbol-concat mode "-map")
            augmap-x   (aph-keys--augment-name mode))
        (error "Triggered error")))
    (should-not (intern-soft mode-x))
    (should-not (intern-soft hook-x))
    (should-not (intern-soft keymap-x))
    (should-not (intern-soft augmap-x))))


;;; Personal Keybindings Mode Tests
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
