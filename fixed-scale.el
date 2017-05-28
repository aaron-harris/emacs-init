;;; fixed-scale.el --- Make text scale stickier      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `seq'

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

;; This module defines `fixed-scale-mode', a global minor mode that
;; attempts to preserve the current text scaling when certain commands
;; change it.
;;
;; To use the mode, just enable `fixed-scale-mode' in the usual way
;; and add commands that you wish preserved text scaling to the
;; variable `fixed-scale-command-list'.
;;
;; If you use a replacement for `execute-extended-command' (M-x), you
;; may need to add it to the list `fixed-scale-extended-command-list'
;; so that `fixed-scale-mode' knows that it should check
;; `extended-command-history' for the actual command being executed.

;;; Code:

(require 'seq)


;;;; User Options
;;===============
(defgroup fixed-scale nil
  "Make text scale stickier."
  :prefix "fixed-scale-"
  :link   '(emacs-commentary-link "fixed-scale")
  :group  'display)

(define-widget 'fixed-scale-command 'lazy
  "A command (interactive function)."
  :offset 4
  :tag    "Command"
  :type   '(restricted-sexp :match-alternatives commandp))

(defcustom fixed-scale-command-list nil
  "A list of commands that should not change text scaling."
  :type '(repeat fixed-scale-command))

(defcustom fixed-scale-extended-command-list
  '(execute-extended-command helm-M-x)
  "A list of commands that execute other commands.

When one of these commands is executed, `fixed-scale-mode'
consults `extended-command-history' to determine whether the
command executed is in `fixed-scale-command-list'."
  :type '(repeat fixed-scale-command))


;;;; State Variables
;;==================
(defvar fixed-scale 0
  "Text scale remembered by `fixed-scale-mode'.")


;;;; Implementation
;;=================
(defun fixed-scale-remember ()
  "Save current text scaling, if necessary.

Intended for use in `pre-command-hook'.  If `this-command' is in
`fixed-scale-command-list', remember the current text scale (as
`fixed-scale') and add `fixed-scale-reset' to `post-command-hook'
so it can be restored."
  (when (require 'validate nil :noerror)
    (validate-variable 'fixed-scale-command-list))
  (when (bound-and-true-p text-scale-mode)
    (setq fixed-scale text-scale-mode-amount)
    (add-hook 'post-command-hook #'fixed-scale-reset)))

(defun fixed-scale-reset ()
  "Restore text scaling from last `fixed-scale-remember'.

Intended for use in `post-command-hook'.  Set the current text
scale to whatever `fixed-scale-remember' remembered (as
`fixed-scale'), and remove self from `post-command-hook'."
  (remove-hook 'post-command-hook #'fixed-scale-reset)
  (when (or (seq-contains fixed-scale-command-list this-command)
            (and (seq-contains fixed-scale-extended-command-list
                               this-command)
                 (seq-contains fixed-scale-command-list
                               (intern-soft (car extended-command-history)))))
    (text-scale-set fixed-scale)))

;;;###autoload
(define-minor-mode fixed-scale-mode
  "Global minor mode to make `text-scale-mode' stickier.

Sometimes a command will remove text scaling as a byproduct of
its function.  This mode attempts to rectify that.  To use it,
you must add commands that reset the text scale to the variable
`fixed-scale-command-list'.  Then, whenever one of those commands
is executed while `fixed-scale-mode' is enabled (it is enabled by
default, if the module `fixed-scale' has been loaded),
`fixed-scale-mode' will remember the proper text scaling and
restore it after the command is finished."
  :global      t
  :init-value  t
  :require     'fixed-scale
  (if fixed-scale-mode
      (add-hook 'pre-command-hook #'fixed-scale-remember)
    (remove-hook 'pre-command-hook  #'fixed-scale-remember)
    (remove-hook 'post-command-hook #'fixed-scale-reset)))

(provide 'fixed-scale)
;;; fixed-scale.el ends here
