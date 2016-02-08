;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; APH-KEYS TESTS
;;;;============================================================================

;; Tests for the module `aph-keys'.
(require 'aph-keys)


;;; Personal Keybindings Mode
;;;==========================
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

      
(provide 'aph-keys-test)
