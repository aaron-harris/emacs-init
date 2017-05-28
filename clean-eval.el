;;; clean-eval.el --- Control format of eval output  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience, lisp

;; Advised functions from other packages:
;;   simple: `eval-expression-print-format'

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

;; This module defines the global minor mode `clean-eval-mode'.  When
;; this mode is enabled, evaluating elisp (e.g., via `eval-expression'
;; or through `ielm') will not include extra "junk" information that
;; it normally does.
;;
;; For example, if you evaluate the form "(+ 1 1)", the normal output
;; displayed is "2 (#o2, #x2, ?\\C-b)".  With this mode enabled, only
;; "2" will be displayed.
;;
;; This mode is based on a Stackexchange answer by user Harald
;; Hanche-Olsen, which can be found here:
;;
;;   http://emacs.stackexchange.com/a/3772/8647

;;; Code:

(defun clean-eval--advice (&rest args)
  "Advice to enforce `clean-eval-mode'.

If `clean-eval-mode' is active, return the empty string.
Otherwise, remove self as advice from
`eval-expression-print-format' and return nil.  In either case,
ignore ARGS.

Intended as :before-until advice on
`eval-expression-print-format'."
  (if clean-eval-mode
      ""
    (remove-advice 'eval-expression-print-format #'clean-eval--advice)
    nil))

;;;###autoload
(define-minor-mode clean-eval-mode
  "Mode to clean evaluation output.

When enabled, evaluating elisp (e.g., with `eval-expression' or
via `ielm') will not produce extra \"junk output\".  For example,
you would see

    (+ 1 1)
    => 2

rather than

    (+ 1 1)
    => 2 (#o2, #x2, ?\\C-b).

Note that this is accomplished via advice on the function
`eval-expression-print-format'."
  :global t
  (if clean-eval-mode
      (advice-add 'eval-expression-print-format
		  :before-until #'clean-eval--advice)
    (advice-remove 'eval-expression-print-format #'clean-eval--advice)))


;;;; Unloading
;;============
(defun clean-eval-unload-function ()
  "Undo changes made to Emacs for `clean-eval-mode'.

More specifically, remove advice supporting `clean-eval-mode'
from the function `eval-expression-print-format'."
  (advice-remove 'eval-expression-print-format #'clean-eval--advice))

(provide 'clean-eval)
;;; clean-eval.el ends here
