;;; proctor.el --- Test apparatus for use with ERT   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `ert-x', `alist', `symbol'

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

;;; Commentary:

;; This module contains macros and functions for general-purpose use
;; inside ERT tests, as fixtures, mocks, etc.
;;
;; Some tests generate temporary files, for testing interaction with
;; the filesystem.  To choose where these files will be created,
;; customize `proctor-directory'; the default is a directory
;; ".proctor" inside `user-emacs-directory'.

;;; Code:

(require 'ert-x)

(require 'alist)
(require 'symbol)

(eval-when-compile (require 'subr-x))


;;;; User Options
;;===============
(defgroup proctor nil
  "Extra ERT test apparatus"
  :prefix "proctor-"
  :link '(emacs-commentary-link "proctor")
  :group 'ert)

(defcustom proctor-directory
  (expand-file-name ".proctor/" user-emacs-directory)
  "Directory that `proctor' should use for temporary files.

These files will be generated as part of individual tests and
should not persist outside of those tests."
  :type 'string)


;;;; Basic Test Wrappers
;;======================
(defmacro proctor-test-all (function test &rest pairs)
  "Test FUNCTION with each of the PAIRS.  Compare with TEST.

Call FUNCTION with the car of each PAIR (a list of arguments),
and check that the value returned is the cdr of that PAIR.

Example usage:

    (proctor-test-all #'+ #'=
        (nil   . 0)
        ((1)   . 1)
        ((1 2) . 3))"
  (declare (debug t)
           (indent 2))
  `(dolist (pair ',pairs)
     (should (funcall ,test
                      (apply ,function (car pair))
                      (cdr pair)))))


;;;; Macro Testing
;;================
(defun proctor-macro-executes-body (macro &optional other-args)
  "Execute a standard test to check that MACRO executes a body.

Apply MACRO to a body form, prepending any OTHER-ARGS, and check
both that this body was executed, and that the value returned is
the return value of its last form.  Return t if both conditions
hold, and signal an error with `should' otherwise."
  (let* ((canary  (make-symbol "canary"))
         (test    `(let (,canary)
                     (should (= 7 (,macro ,@other-args
                                          (setq ,canary t)
                                          (+ 1 6))))
                     (should ,canary))))
    (eval test)))

(defun proctor-macro-does-not-leak (macro var-form &optional other-args)
  "Test to ensure that MACRO does not leak binding for VAR-FORM.

Apply MACRO to a body form, prepending any OTHER-ARGS, and check
that the symbol whose name is given by VAR-FORM inside BODY is
not defined as either a variable or a function after BODY exits.

Note that it is acceptable for VAR-FORM to name a symbol that is
either `boundp' or `fboundp', so long as it is not interned."
  (let* ((var-x  (make-symbol "var-x"))
         (subtest
          (lambda (wrap form)
            `(let (,var-x)
               (,wrap
                (,macro ,@other-args
                        (setq ,var-x ,var-form)
                        ,form)
                (should-not (and (intern-soft ,var-x)
                                 (or (boundp ,var-x) (fboundp ,var-x)))))))))
    (eval (funcall subtest 'progn         '(ignore)))
    (eval (funcall subtest 'ignore-errors '(error "Triggered error"))) 
    t))


;;;; Buffer and File Handling
;;===========================
(defmacro proctor-with-buffer (mode text &rest body)
  "Execute BODY in temp buffer in MODE that contains TEXT.

Create a temporary buffer, insert TEXT, and enable
MODE (typically a major mode).  Move point to the beginning of
this buffer and execute BODY.

The buffer is created with `ert-with-test-buffer', so it will
persist in the event of an error in BODY.

For convenience, if TEXT begins with a newline, that newline is
not included in the buffer text.  This allows for the following
sort of layout, which avoids both problematic indentation and the
need to skip over the leading newline.

  (proctor-with-buffer text-mode \"
Buffer line 1
Buffer line 2\"
    (should (looking-at-p \"Buffer line 1\")))"
  (declare (indent 2)
           (debug t))
  `(ert-with-test-buffer ()
     (insert (string-remove-prefix "\n" ,text))
     (funcall ,mode)
     (goto-char (point-min))
     ,@body))

(defmacro proctor-with-buffer-renamed (buffer &rest body)
  "As `ert-with-buffer-renamed', with an improved signature.

The macro `ert-with-buffer-renamed' has a slightly awkward
signature that is inconsistent with the other `proctor' macros.
This is a thin wrapper around `ert-with-buffer-renamed' that
normalizes the signature.  Contrast:

    (ert-with-buffer-renamed (\"Foo\")
        (do-stuff-here))

with

    (proctor-with-buffer-renamed \"Foo\"
      (do-stuff-here))"
  (declare (indent 1))
  `(ert-with-buffer-renamed (,buffer) ,@body))

(defmacro proctor-with-buffers-renamed (buffers &rest body)
  "As `proctor-with-buffer-renamed', for multiple BUFFERS.

BUFFERS should be a form returning a list of buffer names.  Each
buffer will be protected as in `ert-with-buffer-renamed'."
  (declare (indent 1))
  (let ((buffer-list (make-symbol "buffer-list")))
    `(let ((,buffer-list  ,buffers))
       (if (null ,buffer-list)
           (progn ,@body)
         (ert-call-with-buffer-renamed
          (car ,buffer-list)
          (lambda ()
            (macroexpand '(proctor-with-buffers-renamed
                              (cdr ,buffer-list))) ,@body))))))

(defmacro proctor-with-file (file text &rest body)
  "Create FILE with TEXT, evaluate BODY, then delete FILE.

If FILE is relative, it is created inside `proctor-directory'.
If its parent directory does not already exist, it is created,
but that directory will not be deleted after BODY exits.

As with `proctor-with-buffer', if TEXT begins with a newline,
that newline is not included in the file text.

Note that the current buffer is not changed, and FILE is not
guaranteed to be open in any buffer."
  (declare (indent 2)
           (debug t)) 
  (let ((abs-file  (make-symbol "abs-file"))
        (file-dir  (make-symbol "file-dir")))
    `(let* ((,abs-file  (expand-file-name ,file proctor-directory))
            (,file-dir  (file-name-directory ,abs-file)))
       (unwind-protect
           (progn (unless (file-exists-p ,file-dir)
                    (make-directory ,file-dir))
                  (with-temp-file ,abs-file
                    (insert (string-remove-prefix "\n" ,text)))
                  ,@body)
         (when (file-exists-p ,abs-file)
           (delete-file ,abs-file))))))


;;;; Temporary Modes
;;==================
(defmacro proctor--with-temp-mode (name maker &rest body)
  "Subroutine used to generate temporary modes for testing.

More specifically:

- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and call MAKER (a function) with that name as an argument.
  MAKER should create a mode (either major or minor) in some way.

- Execute BODY, with the name of the new mode bound to NAME (a
  symbol).  Also bind the hook variable associated with the mode to
  the symbol NAME-hook, and the keymap to NAME-map.

- Make sure the mode created does not persist outside this form,
  using `unwind-protect' to ensure it is deleted in the event of
  an error or nonlocal exit from BODY."
  (declare (debug (symbolp form body))
           (indent 2))
  (let ((hook        (symbol-concat name "-hook"))
        (keymap-var  (make-symbol "keymap-var"))
        (keymap      (symbol-concat name "-map")))
    `(let* ((,name        (cl-gensym "mode"))
            (,hook        (symbol-concat ,name "-hook"))
            (,keymap-var  (symbol-concat ,name "-map")) 
            ,keymap)
       (unwind-protect
           (progn (funcall ,maker ,name)
                  (setq ,keymap (symbol-value ,keymap-var))
                  ,@body)
         (unintern ,hook)
         (unintern ,keymap-var)
         (unintern (symbol-concat ,name "-syntax-table"))
         (unintern (symbol-concat ,name "-abbrev-table"))))))

(defun proctor-test-mode-wrapper-bindings (macro &optional other-args)
  "Test that MACRO binds variables per `proctor--with-temp-mode'.

Here MACRO should be a symbol naming a macro with the same
general contract as `proctor--with-temp-mode'.  That is:

- MACRO should have a signature like
  (MACRO NAME [OTHER-ARGS ...] &rest BODY).

- Inside BODY, the variables NAME, NAME-hook, and NAME-map should be
  respectively bound to a mode of some kind, that mode's hook
  variable, and the mode's keymap.

- The above bindings should not persist outside of BODY.

Run a test confirming that the bindings are all appropriately
made inside BODY.  If they are, return t.  Otherwise, signal an
error using `should'."
  (let ((test
         `(,macro mode ,@other-args
                  (should (fboundp mode))
                  (should (eq mode-hook
                              (symbol-concat mode "-hook")))
                  (should (boundp mode-hook))
                  (should (eq mode-map
                              (symbol-value (symbol-concat mode "-map"))))
                  (should (keymapp mode-map)))))
    (eval test)))

(defun proctor-test-mode-wrapper-cleanup (macro &optional other-args)
  "Test that MACRO cleans up after itself.

Here MACRO should be a symbol naming a macro with the same
general contract as `proctor--with-temp-mode'.  That is:

 - MACRO should have a signature like
  (MACRO NAME [OTHER-ARGS ...] &rest BODY).

- Inside BODY, the variables NAME, NAME-hook, and NAME-map should be
  respectively bound to a mode of some kind, that mode's hook
  variable, and the mode's keymap.

- The above bindings should not persist outside of BODY.

Run a test confirming that the bindings do not persist after BODY
exits.  If they do not, return t.  Otherwise, signal an error
using `should'."
  (let ((args      (cons 'mode other-args))
        (var-forms '(mode
                     mode-hook
                     (symbol-concat mode "-map"))))
    (dolist (v var-forms t)
      (should (proctor-macro-does-not-leak macro v args)))))

(defmacro proctor-with-major-mode (name parent &rest body)
  "Execute BODY in an environment with a temporarily-defined major mode.

More specifically:

- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and create a major mode (using `define-derived-mode') with this
  name.  Evaluate PARENT to get the name of the parent mode.

- Execute BODY, with the name of the new mode bound to NAME (a
  symbol).  Also bind the hook variable associated with the mode to
  the symbol NAME-hook, and the keymap to NAME-map.

- Make sure the mode created does not persist outside this form,
  using `unwind-protect' to ensure it is deleted in the event of
  an error or nonlocal exit from BODY.

Note that the major mode constructed in this block doesn't actually do
anything (i.e., its body is empty)."
  (declare (debug (symbolp form body))
           (indent 2))
  `(proctor--with-temp-mode ,name
       (lambda (child)
         (eval `(define-derived-mode ,child ,,parent "Lighter")))
     ,@body))

(defmacro proctor-with-minor-mode (name &rest body)
  "Execute BODY in an environment with a temporarily-defined minor mode.

More specifically:

- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and create a minor mode (using `define-minor-mode') with this
  name.

- Execute BODY, with the name of the new mode bound to NAME (a
  symbol).  Also bind the hook variable associated with the mode to
  the symbol NAME-hook, and the keymap to NAME-map.

- Make sure the mode created does not persist outside this form, using
  `unwind-protect' to ensure it is deleted in the event of an error or
  nonlocal exit from BODY.

Note that the minor mode constructed in this block doesn't actually do
anything (i.e., its body is empty)."
  (declare (debug (symbolp body))
           (indent 1))
  (let ((mode-name  (make-symbol "mode-name")))
    `(let (,mode-name)
       (proctor--with-temp-mode ,name
           (lambda (mode)
             (eval `(define-minor-mode ,mode "Doc"
                      :keymap (make-sparse-keymap))))
         (unwind-protect (progn (setq ,mode-name ,name)
                                ,@body)
           (alist-delete minor-mode-map-alist ,mode-name))))))

(provide 'proctor)
;;; proctor.el ends here
