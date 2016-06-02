;;; org-display.el --- Control Org window positions  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience, org

;; Dependencies: `validate' (optional)
;; Advised functions from other packages:
;;   org: `org-capture', `org-fast-todo-selection'

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

;; By default, `org-mode' is rather heavy-handed in deciding where its
;; "control buffers" (such as the tag selection buffer or the capture
;; buffer) should appear.  This module attempts to give the user more
;; control over the placement of these buffers.
;;
;; Each type of control buffer can be position independently using a
;; different control variable; see below for a complete list.  To
;; control where a type of buffer is placed, find the appropriate
;; control variable and set it to a suitable "action" for use by
;; `display-buffer'.  See the docstring for `display-buffer' for more
;; information on what this means; most often, this will be a
;; singleton list containing a function like
;; `display-buffer-pop-up-window' or `display-buffer-in-side-window'.
;;
;; Here is a complete list of control variables, and the buffers they
;; control:
;;
;; `org-display-capture-placement-action':
;;
;;     This variable controls the placement of all buffers used by
;;     `org-capture'.
;;
;; `org-display-todo-placement-action':
;;
;;     This variable controls the fast todo selection buffer presented
;;     by `org-todo' when the option `org-use-fast-todo-selection' is
;;     non-nil.

;;; Code:

(eval-when-compile (require 'cl-lib))


;;;; User Options
;;===============
(defgroup org-display nil
  "Control where Org windows appear."
  :prefix "org-display-"
  :link '(emacs-commentary-link "org-display")
  :group 'org)

(define-widget 'org-display-action 'lazy
  "An action for use with `display-buffer'.
This is a cons with car a function or list of functions and cdr
an arbitrary alist."
  :tag "Buffer display action"
  :type '(cons (choice function (repeat function))
	       (alist :key-type sexp :value-type sexp)))

(defcustom org-display-capture-placement-action nil
  "Action to use for placement of capture buffer.

This affects all buffers displayed during the capture process.

If nil, do not override Org-mode's default behavior.  Otherwise,
attempt to use the specified value (a pair (FUNCTION . ALIST)) as
a `display-buffer' action; see the docstring for `display-buffer'
for more information."
  :type 'org-display-action
  :require 'org-display)

(defcustom org-display-todo-placement-action nil
  "Action to use for placement of todo selection buffer.

This affects the buffer displayed by `org-todo' when the option
`org-use-fast-todo-selection' is non-nil.

If nil, do not override Org-mode's default behavior.  Otherwise,
attempt to use the specified value (a pair (FUNCTION . ALIST)) as
a `display-buffer' action; see the docstring for `display-buffer'
for more information."
  :type 'org-display-action
  :require 'org-display)


;;;; Implementation
;;=================
(defun org-display-placement-advice-core
    (orig-fn control-var buffer-re &rest args)
  "Core subroutine for `org-display' module.

Call ORIG-FN with ARGS, overriding `org-mode''s hard-coded
positioning logic and using the value of CONTROL-VAR as a
`display-buffer' action for buffers matching the regexp
BUFFER-RE."
  (if (not (symbol-value control-var)) (apply orig-fn args)
    (when (require 'validate nil :noerror)
      (validate-variable control-var))
    (cl-letf*
	(((symbol-function 'org-switch-to-buffer-other-window)
	  #'pop-to-buffer)

	 (display-buffer-alist
	  (cons `(,buffer-re ,(symbol-value control-var))
		display-buffer-alist)))
      (apply orig-fn args))))

(defun org-display-capture-placement-advice (orig-fn &optional goto keys)
  "Advice to enforce `org-display-capture-placement-action'.
Intended as :around advice for `org-capture'."
  (if goto (funcall orig-fn goto keys)
    (org-display-placement-advice-core
     orig-fn 'org-display-capture-placement-action
     "\\*Org Select\\*\\|\\*Capture\\*\\|CAPTURE-.*"
     goto keys)))

(advice-add 'org-capture
	    :around #'org-display-capture-placement-advice)

(defun org-display-todo-placement-advice (orig-fn)
  "Advice to enforce `org-display-todo-placement-action'.
Intended as :around advice for `org-fast-todo-selection'." 
  (org-display-placement-advice-core
   orig-fn 'org-display-todo-placement-action
   "\\*Org todo\\*"))

(advice-add 'org-fast-todo-selection
	    :around #'org-display-todo-placement-advice)


;;;; Unloading
;;============
(defun org-display-unload-function ()
  "Unload changes made to Emacs by `org-display' module.

This consists of removing the following pieces of advice:

- `org-display-capture-placement-advice' on `org-capture'.

- `org-display-todo-placement-advice' on `org-fast-todo-selection'."
  (let (advice-list
	'((org-capture	           . org-display-capture-placement-advice)
	  (org-fast-todo-selection . org-display-todo-placement-advice)))
    (dolist (pair advice-list)
      (advice-remove (car pair) (cdr pair)))))

(provide 'org-display)
;;; org-display.el ends here
