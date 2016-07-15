;;; aph-vc-test.el --- Tests for aph-vc.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-vc', `ert'

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

(require 'aph-vc)
(eval-when-compile (require 'cl-lib))


;;;; Test Apparatus
;;=================
(defvar aph/vc-test-file "aph-vc-test.temp"
  "The filename to use for testing functions from `aph-vc'.")

(defvar aph/vc-test-canary nil
  "Variable used for reporting by testing mocks for `aph-vc'.")

(defvar aph/vc-test-use-vc nil
  "Variable used by `aph-vc' mock for `vc-backend'.")

(defvar aph/vc-test-confirm-deletion nil
  "Variable used to bypass `y-or-n-p' in `aph-vc' mocks.")

(defun aph/vc-test-delete-mock (file)
  "Testing mock for `vc-delete-file'.

If FILE is under version control, as reported by `vc-backend',
delete it using `delete-file' and set `aph/vc-test-canary' to the
keyword :vc-delete to indicate that this function has been
called.

If FILE is not under version control, signal an error in the same
way that `vc-delete-file' would."
  (if (vc-backend file)
      (progn (delete-file file)
             (setq aph/vc-test-canary :vc-delete))
    (error "File %s is not under version control"
           (file-name-nondirectory file))))

(defmacro aph/vc-test-delete-fixture (&rest body)
  "Fixture for testing `aph-vc' functions.

Execute BODY in an environment suitable for testing functions
from the module `aph-vc'.  In this environment:

* The function `vc-delete-file' is replaced by the mock
  `aph/vc-test-delete-mock'; see the latter function's
  documentation for more details.

* The functions `vc-backend' and `y-or-n-p' are mocked by
  constant functions that return the values of the variables
  `aph/vc-test-use-vc' and `aph/test-confirm-deletion',
  respectively.

* The variable `aph/vc-test-canary' (which is used by
  `aph/vc-test-delete-mock') is reset to nil.

* A file exists at the filename `aph/vc-test-file'.  If this file
  still exists after BODY finishes, it will be deleted.  This is
  still true in the event of an error or nonlocal exit."
  (declare (indent 0))
  `(cl-letf*
       (((symbol-function 'vc-delete-file)
         #'aph/vc-test-delete-mock)
        ((symbol-function 'vc-backend)
         (lambda (_) aph/vc-test-use-vc))
        ((symbol-function 'y-or-n-p)
         (lambda (_) aph/vc-test-confirm-deletion)))
     (unwind-protect
         (progn (setq aph/vc-test-canary nil)
                (with-temp-file aph/vc-test-file (insert "Foo"))
                ,@body)
       (when (file-exists-p aph/vc-test-file)
         (delete-file aph/vc-test-file)))))

(ert-deftest aph/vc-test-delete-fixture ()
  "Test that `aph/vc-test-delete-fixture' works correctly."
  (aph/vc-test-delete-fixture
    (should-not aph/vc-test-canary)
    (should (file-exists-p aph/vc-test-file))
    (should (eq (symbol-function 'vc-delete-file)
                #'aph/vc-test-delete-mock))
    (dolist (aph/vc-test-use-vc '(nil t))
      (should (eq (vc-backend aph/vc-test-file) aph/vc-test-use-vc)))
    (dolist (aph/vc-test-confirm-deletion '(nil t))
      (should (eq (y-or-n-p "Prompt") aph/vc-test-confirm-deletion)))
    (setq aph/vc-test-canary t))
  (should aph/vc-test-canary)
  (should-not (file-exists-p aph/vc-test-file)))

(ert-deftest aph/vc-test-delete:vc ()
  "Test `aph/vc-delete-file' with version control."
  (aph/vc-test-delete-fixture
    (setq aph/vc-test-use-vc t)
    (aph/vc-delete-file aph/vc-test-file)
    (should-not (file-exists-p aph/vc-test-file))
    (should (eq aph/vc-test-canary :vc-delete))))

(ert-deftest aph/vc-test-delete:no-vc ()
  "Test `aph/vc-delete-file' outside of version control."
  (aph/vc-test-delete-fixture
    (setq aph/vc-test-use-vc           nil
          aph/vc-test-confirm-deletion t)
    (aph/vc-delete-file aph/vc-test-file)
    (should-not (file-exists-p aph/vc-test-file))
    (should (eq aph/vc-test-canary nil))))

(ert-deftest aph/vc-test-delete:no-confirm ()
  "Test `aph/vc-delete-file' while denying confirmation."
  (aph/vc-test-delete-fixture
    (setq aph/vc-test-use-vc           nil
          aph/vc-test-confirm-deletion nil)
    (aph/vc-delete-file aph/vc-test-file)
    (should (file-exists-p aph/vc-test-file))))

(provide 'aph-vc-test)
;;; aph-vc-test.el ends here
