;;; fix.el --- Named partial application             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: lisp

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

;; Suppose you want to add a function to a hook, but you need to
;; supply one or more arguments; that is, you want to do something
;; like this:
;;
;;     (add-hook 'foo-hook (lambda () (foo :bar)))
;;
;; This is fine, except that use of lambdas in hooks is discouraged,
;; because it interferes with the ability to remove hooks easily.  So
;; you would probably do this instead:
;;
;;    (defun foo-bar ()
;;      "As `foo', with fixed first arg :bar."
;;      (foo :bar))
;;
;;    (add-hook 'foo-hook #'foo-bar)
;;
;; Now you have this ugly boilerplate function definition cluttering
;; up your init file.  This module allows you to replace the
;; function definition with an inline call like this:
;;
;;     (add-hook 'foo-hook (fix-args #'foo "bar" :bar))
;;
;; This will create a named function `foo:bar' that does exactly what
;; you want, without cluttering up your init file.
;;
;;
;; Often, the function that needs to be modified is a minor mode;
;; usually this is so you can turn the mode off in a hook.  For this
;; usage, see the function `fix-mode-off'.

;; A similar command `fix-mode-on' is also provided, although this is
;; less-used, since minor modes generally turn themselves on
;; unconditionally when called with no arguments anyway.

;;; Code:


;;;; Subroutines
;;==============
(defun fix--name (symbol suffix)
  "Return the symbol obtained from SYMBOL by appending SUFFIX.

A colon is interposed between SYMBOL and SUFFIX."
  (intern (concat (symbol-name symbol) ":" suffix)))


;;;; Partial Application
;;======================
(defun fix-args--doc (fun &rest args)
  "Generate docstring for `fix-args' function."
  (concat
   (format "As `%s', with fixed " fun)
   (cond
    ((= 1 (length args))
     (format "first arg %s." (car args)))
    ((= 2 (length args))
     (format "first args %s, %s." (nth 0 args) (nth 1 args)))
    (:else
     (concat "with fixed arguments.\n"
             (format "The arglist supplied is %s." args))))))

(defun fix-args (fun suffix &rest fixed-args)
  "Create and return a function applying FUN to FIXED-ARGS.

When supplied with a symbol FUN, a string SUFFIX, and any number
of ARGS, define a new function whose name is obtained from FUN by
appending \":SUFFIX\".  This function is the same as that
produced by `apply-partially'.

Return the function just defined."
  (eval `(defun ,(fix--name fun suffix) (&rest args)
           ,(fix-args--doc fun fixed-args)
           (apply (apply-partially #',fun ,@fixed-args) args))))


;;;; Mode Switches
;;================
(defun fix-mode-on (mode)
  "Create and return a function that turns on MODE.

Here MODE should be a minor mode."
  (eval `(defun ,(fix--name mode "on") ()
           ,(format "Turn on the minor mode `%s' unconditionally." mode)
           (interactive)
           (,mode 1))))

(defun fix-mode-off (mode)
  "Create and return a function that turns off MODE.

Here MODE should be a minor mode."
  (eval `(defun ,(fix--name mode "off") ()
           ,(format "Turn off the minor mode `%s' unconditionally." mode)
           (interactive)
           (,mode -1))))

(provide 'fix)
;;; fix.el ends here
