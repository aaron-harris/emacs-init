;;; chimera-test.el --- Tests for chimera.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `chimera', `ert-x', `proctor'
;; Optional dependencies: `bind-key'

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

(require 'chimera)
(require 'ert-x)
(require 'proctor)

;; Test `bind-key' support if possible.
(require 'bind-key nil :noerror)


;;; Test Apparatus
;;;===============
(defmacro chimera-test (&rest body)
  "Test that BODY correctly binds the test chimera.

All of the tests in the `chimera-test' module are of the same
format.  This macro consolidates the boilerplate code for those
tests.

The code in BODY should bind the testing chimera

    (chimera \"chimera/test\"
      (when case-fold-search 'chimera))

to the key \"a\" in the keymap named `chimera-test-mode-map'.
The name of this keymap variable is also available as
`chimera-test-mode-map-name'.

\(The testing chimera is not available as a variable, or similar,
because we want to make sure this kind of inline use is fully
supported, e.g. by `bind-key', and the extra level of indirection
makes that difficult.)

This macro will do the rest."
  (declare (indent 0)
           (debug body))
  `(proctor-with-major-mode ambient-mode 'fundamental-mode
     (proctor-with-minor-mode chimera-test-mode
       (let ((chimera-test-mode-map-name
              (intern (concat (symbol-name chimera-test-mode) "-map")))
             case-fold-search
             personal-keybindings)
         (with-temp-buffer
           (funcall ambient-mode)
           (funcall chimera-test-mode 1)
           ;; Set up keybindings
           (define-key ambient-mode-map (kbd "a") 'default)
           ,@body
           ;; Test keybindings
           (should (eq 'default (key-binding (kbd "a"))))
           (setq case-fold-search t)
           (should (eq 'chimera (key-binding (kbd "a"))))
           ;; `personal-keybindings' should not error
           (ert-with-buffer-renamed ("*Personal Keybindings*")
             (and (fboundp 'describe-personal-keybindings)
                  (describe-personal-keybindings))
             :success))))))


;;; Tests
;;;======
(ert-deftest chimera-test-define-key ()
  "Test `chimera' with `define-key'."
  (chimera-test
   (define-key chimera-test-mode-map (kbd "a")
     (chimera "chimera/test"
       (when case-fold-search 'chimera)))))

(ert-deftest chimera-test-bind-key ()
  "Test `chimera' with `bind-key'."
  (skip-unless (featurep 'bind-key))
  (chimera-test
    (eval `(bind-key "a"
                     (chimera "chimera/test"
                       (when case-fold-search 'chimera))
                     ,chimera-test-mode-map-name))))

(ert-deftest chimera-test-bind-keys ()
  "Test `chimera' with `bind-keys'."
  (skip-unless (featurep 'bind-key))
  (chimera-test
    (eval `(bind-keys :map ,chimera-test-mode-map-name
                      ("a" . (chimera "chimera/test"
                               (when case-fold-search 'chimera)))))))


(provide 'chimera-test)
;;; chimera-test.el ends here
