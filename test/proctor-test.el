;;; proctor-test.el --- Tests for proctor.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `proctor', `bfw'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'proctor)

(require 'bfw)


;;;; Basic Test Wrappers
;;======================
(ert-deftest proctor-test-all ()
  "Test `proctor-test-all'."
  (proctor-test-all #'+ #'=
    (nil   . 0)
    ((1)   . 1)
    ((1 2) . 3)
    ((1 2) . ,(* 1 3))))


;;;; Random Testing
;;=================
(ert-deftest proctor-test-random ()
  "Test `proctor-random'." 
  (proctor-random 10000 100
      ((0 . 2500) (1 . 2500) (2 . 2500) (3 . 2500))
    (random 4))
  (should-error (proctor-random 10000 0
      ((0 . 2500) (1 . 2500) (2 . 2500) (3 . 2500))
    (random 4))))


;;;; Macro Testing
;;================
(ert-deftest proctor-test-macro-executes-body ()
  "Test `proctor-macro-executes-body'."
  (should (proctor-macro-executes-body 'with-temp-buffer))
  (should (proctor-macro-executes-body 'let '((canary))))
  (should-error (proctor-macro-executes-body 'ignore)))

(ert-deftest proctor-test-macro-does-not-leak ()
  "Test `proctor-macro-does-not-leak-p'."
  (should (proctor-macro-does-not-leak 'let 'var-x '((var-x))))
  (should-error (proctor-macro-does-not-leak-p
                 'let ''emacs-version '((var-x)))))


;;;; Buffer and File Handling
;;===========================
(ert-deftest proctor-test-with-buffer ()
  "Test `proctor-with-buffer'."
  (should (proctor-macro-executes-body 'proctor-with-buffer
                                       '('text-mode "Foo")))
  (dolist (text '("Foo" "\nFoo"))
    (proctor-with-buffer 'text-mode text
      (should (eq major-mode 'text-mode))
      (should (looking-at-p "Foo")))))

(ert-deftest proctor-test-with-buffers-renamed ()
  "Test `proctor-with-buffers-renamed'."
  (should (proctor-macro-executes-body 'proctor-with-buffers-renamed
                                       '((list "*Messages*"))))
  (let* ((buf-foo   (get-buffer-create "Foo"))
         (buf-bar   (get-buffer-create "Bar"))
         (name-foo  (buffer-name buf-foo))
         (name-bar  (buffer-name buf-bar)))
    (unwind-protect
        (proctor-with-buffers-renamed
            (list name-foo name-bar)
          (should-not (buffer-live-p name-foo))
          (should-not (buffer-live-p name-bar)))
      (bfw-kill-buffer-if-any buf-foo)
      (bfw-kill-buffer-if-any buf-bar))))

(ert-deftest proctor-test-with-file ()
  "Test `proctor-with-file'."
  (should (proctor-macro-executes-body 'proctor-with-file
                                       '("foo" "Foo")))
  (let ((abs-file  (expand-file-name "foo" proctor-directory)))
    (dolist (text '("Foo" "\nFoo"))
      (proctor-with-file "foo" text
        (should (file-exists-p abs-file))
        (with-temp-buffer
          (insert-file-contents abs-file)
          (should (equal (buffer-string) "Foo"))))
      (should-not (file-exists-p abs-file)))))

(ert-deftest proctor-test-with-file:buffer ()
  "Test `proctor-with-file' with an open buffer."
  (let (buffer)
    (unwind-protect
        (progn
          (proctor-with-file "foo" "Foo"
            (setq buffer
                  (find-file (expand-file-name "foo" proctor-directory))))
          (should-not (buffer-live-p buffer)))
      (bfw-kill-buffer-if-any buffer))))

(ert-deftest proctor-test-with-file:modified ()
  "Test `proctor-with-file' when buffer is modified.

The macro `proctor-with-file' should kill any buffer visiting the
temporary file without prompting when its body exits, even if
that buffer is modified.  Therefore, we consider any call to
`y-or-n-p' or `yes-or-no-p' will fail this test."
  (cl-letf (((symbol-function #'y-or-n-p)    #'error)
            ((symbol-function #'yes-or-no-p) #'error)
            (buffer nil))
    (proctor-with-file "temp" "Foo"
      (setq buffer (find-file (expand-file-name "temp" proctor-directory)))
      (with-current-buffer buffer
        (insert "Bar")
        (should (buffer-modified-p buffer))))
    (should-not (buffer-live-p buffer))))


;;;; Temporary Modes
;;==================
(ert-deftest proctor-test-with-major-mode ()
  "Test `proctor-with-major-mode'."
  (should (proctor-macro-executes-body
           'proctor-with-major-mode '(mode 'fundamental-mode)))
  (should (proctor-test-mode-wrapper-bindings
           'proctor-with-major-mode '('fundamental-mode)))
  (should (proctor-test-mode-wrapper-cleanup
           'proctor-with-major-mode '('fundamental-mode)))
  ;; Test that modes can be nested
  (proctor-with-major-mode mode1 'text-mode
    (proctor-with-major-mode mode2 mode1
      (should (eq 'text-mode (get mode1 'derived-mode-parent)))
      (should (eq mode1      (get mode2 'derived-mode-parent))))))

(ert-deftest proctor-test-with-minor-mode ()
  "Test `proctor-with-minor-mode'."
  (should (proctor-macro-executes-body 'proctor-with-minor-mode '(mode)))
  (should (proctor-test-mode-wrapper-bindings 'proctor-with-minor-mode))
  (should (proctor-test-mode-wrapper-cleanup  'proctor-with-minor-mode))
  ;; Check for presence of minor mode control variable:
  (proctor-with-minor-mode mode
    (should (boundp mode)))
  ;; Make sure `minor-mode-map-alist' gets cleaned up, too:
  (let (mode-x)
    (proctor-with-minor-mode mode
      (setq mode-x mode)
      (should (assoc mode-x minor-mode-map-alist)))
    (should-not (assoc mode-x minor-mode-map-alist))))

(provide 'proctor-test)
;;; proctor-test.el ends here
