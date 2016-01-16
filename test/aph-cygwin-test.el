;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CYGWIN TESTS
;;;;============================================================================

;; Tests for the module `aph-cygwin'.
(require 'aph-cygwin)


;;; Path Conversion Tests
;;;======================
(ert-deftest aph/cygwin-test-hybrid-conversion ()
  "Test `aph/cygwin-convert-file-name-to-hybrid-windows'."
  (let* ((root (aph/cygwin-convert-file-name-to-hybrid-windows "/"))
         (home (aph/cygwin-convert-file-name-to-hybrid-windows "~")))
    (should (stringp root))
    (should (equal (aph/cygwin-convert-file-name-to-hybrid-windows
                    "/foo/bar/baz.el")
                   (concat root "/foo/bar/baz.el")))
    (should (equal (aph/cygwin-convert-file-name-to-hybrid-windows
                    "~/foo/bar/baz.el")
                   (concat home "/foo/bar/baz.el")))))


(provide 'aph-cygwin-test)
