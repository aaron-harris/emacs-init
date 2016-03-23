;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; APH-KEYS TESTS
;;;;============================================================================

;; Tests for the module `aph-keys'.
(require 'aph-keys)
(require 'aph-ert)
(require 'aph-ert-test)


;;; Conditional Keybinding Tests
;;;=============================
(ert-deftest aph/keys-test-conditional-binding-format ()
  "Test that conditional bindings are constructed properly."
  (let ((binding (aph/keys--construct-conditional-binding ''foo 'toggle))
        filter)
    (should (listp binding))
    (should (eq (car binding) 'menu-item))
    (should (equal (nth 1 binding) "maybe-foo"))
    (should (null (nth 2 binding)))
    (should (eq (nth 3 binding) :filter))
    (setq filter (nth 4 binding))
    (should (functionp filter))
    ;; The `eval' indirection is necessary to escape lexical scoping.
    (should (null (eval `(let ((toggle nil)) (funcall ,filter)))))
    (should (eq 'foo (eval `(let ((toggle t)) (funcall ,filter)))))))

(ert-deftest aph/keys-test-conditional-definition ()
  "Test `aph/keys-define-conditionally'."
  (let ((foo-map           (make-sparse-keymap))
        (case-fold-search  t))
    (aph/keys-define-conditionally foo-map (kbd "a") #'foo
      (not case-fold-search))
    (should-not (lookup-key foo-map (kbd "a")))
    (setq case-fold-search nil)
    (should (eq 'foo (lookup-key foo-map (kbd "a"))))))

(ert-deftest aph/keys-test-conditional-binding ()
  "Test `aph/keys-bind-key-conditionally'."
  (aph/ert-with-minor-mode foo-mode
    (let ((case-fold-search     t)
          (personal-keybindings nil))
      (aph/bind-key-conditionally "a" foo foo-mode-map
        (not case-fold-search))
      (should-not (lookup-key foo-mode-map (kbd "a")))
      (setq case-fold-search nil)
      (should (eq 'foo (lookup-key foo-mode-map (kbd "a")))))
    (should (aph/ert-protecting-buffer "*Personal Keybindings*"
              (describe-personal-keybindings)
              :success))))

(ert-deftest aph/keys-test-bind-keys--when ()
  "Test `bind-keys' support for the :when keyword."
  (aph/ert-with-minor-mode foo-mode
    (let ((case-fold-search     t)
          (personal-keybindings nil))
      (bind-keys :map foo-mode-map
                 ("a" . foo)
                 :when (not case-fold-search)
                 ("b" . bar))
      (should (eq 'foo (lookup-key foo-mode-map (kbd "a"))))
      (should-not (lookup-key foo-mode-map (kbd "b")))
      (setq case-fold-search nil)
      (should (eq 'bar (lookup-key foo-mode-map (kbd "b")))))
    (should (aph/ert-protecting-buffer "*Personal Keybindings*"
              (describe-personal-keybindings)
              :success))))


(provide 'aph-keys-test)
