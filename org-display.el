;;; org-display.el --- Control Org window positions  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience, org

;; Dependencies: `validate' (optional)
;; Advised functions from other packages:
;;   org: `org-fast-todo-selection'

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
;; control variable and set it to a function usable as an "action" by
;; `display-buffer'.  These are functions like
;; `display-buffer-pop-up-window' and `display-buffer-in-side-window'.
;;
;; Here is a complete list of control variables, and the buffers they
;; control:
;;
;; `org-display-todo-placement-function':
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

(defcustom org-display-todo-placement-function nil
  "Function to use for placement of todo selection window.

If nil, do not override Org-mode's default behavior.  Otherwise,
attempt to use the specified function as a `display-buffer'
action; see the docstring for `display-buffer' for more
information."
  :type 'function
  :require 'org-display)


;;;; Implementation
;;=================
(defun org-display-todo-placement-advice (orig-fn)
  "Advice to enforce `org-display-todo-placement-function'.
Intended as :around advice for `org-fast-todo-selection'."
  (if (not org-display-todo-placement-function) (funcall orig-fn)
    (when (require 'validate nil :noerror)
      (validate-variable 'org-display-todo-placement-function))
    (cl-letf*
	(((symbol-function 'org-switch-to-buffer-other-window)
	  #'pop-to-buffer)

	 (display-buffer-alist
	  (cons `("\\*Org todo\\*" ,org-display-todo-placement-function)
		display-buffer-alist)))
      (funcall orig-fn))))

(advice-add 'org-fast-todo-selection
	    :around #'org-display-todo-placement-advice)


;;;; Unloading
;;============
(defun org-display-unload-function ()
  "Unload changes made to Emacs by `org-display' module.

This consists of removing the following pieces of advice:

- `org-display-todo-placement-advice' on `org-fast-todo-selection'."
  (advice-remove 'org-fast-todo-selection #'org-display-todo-placement-advice))

(provide 'org-display)
;;; org-display.el ends here
