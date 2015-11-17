;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MODE TAG TESTS
;;;;============================================================================

;; Tests for the module aph-mode-tag.el.

(ert-deftest aph/mode-tag-test-create--basic ()
  "Test basic functionality of `aph/mode-tag-create'."
  (require 'cl-lib)
  (let* ((tag        (cl-gensym "tag"))
         (docstring  "doc")
         (hook       (intern (concat (symbol-name tag) "-tag-hook"))))
    (unwind-protect
        (progn (eval `(aph/mode-tag-create ,tag ,docstring))
               (should (get tag 'aph/mode-tag))
               (should (equal docstring (get tag 'aph/mode-tag-docstring)))
               (should (boundp hook))
               (should (null (eval hook))))
      (setplist tag nil)
      (makunbound hook)
      (put hook 'variable-documentation nil))))

(provide 'aph-mode-tag-test)
