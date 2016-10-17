;;; family-local.el --- Set variables per family     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions

;; Dependencies: `map', `mode-family'

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

;; This module adds support for "family-local variables" to the mode
;; families introduced by the `mode-family' module.  These act
;; similarly to mode-local variables but apply to all modes in a given
;; mode family.
;;
;; To use this module, just define a mode family as usual, then use
;; the `setq-family-local' macro to set variables, like this:
;;
;;     (mode-family-create 'foo)
;;     (setq-family-local foo
;;       bar 1
;;       baz nil)
;;
;; Now the variable `bar' will be set locally to 1 and `baz' to nil in
;; all modes belonging to the `foo' mode family.

;;; Code:

(require 'mode-family)
(require 'map)


;;;; State Variables
;;==================
(defvar family-local--alist nil
  "An alist mapping mode families to their local variables.

Each element should be of the form (FAMILY . BINDINGS), where
MODE is the name of a mode family and BINDINGS is a plist
associating symbols (variable names) to their desired values in
FAMILY.  The intent is that these variables will have these
buffer-local values in all buffers of all modes in FAMILY.

Do not modify this list directly; use `setq-family-local'
instead.")


;;;; Accessors
;;============
;;;###autoload
(defmacro setq-family-local (family &rest bindings)
  "Set each VAR to VALUE in FAMILY.

\(fn FAMILY [VAR VALUE] ...)"
  (declare (debug (symbolp &rest [symbolp form]))
	   (indent 1))
  (let ((plist    (make-symbol "plist"))
	(setters  nil))
    (while bindings
      (push `(setq ,plist
		   (plist-put ,plist ',(pop bindings) ,(pop bindings)))
	    setters))
    `(let ((,plist (assoc-default ',family family-local--alist #'eq)))
       ,@(reverse setters)
       (map-put family-local--alist ',family ,plist))))


;;;; Implementation
;;=================
(defun family-local-activate ()
  "Activate family-local variables for the current major mode."
  (let (var-plist)
    (dolist (fam (mode-family-list-families major-mode :inherit))
      (setq var-plist (assoc-default fam family-local--alist #'eq))
      (while var-plist
	(eval `(setq-local ,(pop var-plist) ',(pop var-plist)))))))

(add-hook 'after-change-major-mode-hook #'family-local-activate)


;;;; Unloading
;;============
(defun family-local-unload-function ()
  "Undo changes made to support family-local variables.

More specifically, remove `family-local-activate' from
`after-change-major-mode-hook'."
  (remove-hook 'after-change-major-mode-hook #'family-local-activate))

(provide 'family-local)
;;; family-local.el ends here
