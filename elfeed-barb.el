;;; elfeed-barb.el --- More hooks for Elfeed         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience, rss

;; Dependencies: `elfeed', `validate'
;; Advised functions from other packages:
;;   elfeed: `elfeed-show-entry'

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

;; This module adds more hooks to `elfeed' functions.  Note that these
;; are injected using advice, contrary to the usual admonition against
;; advising other packages' functions.
;;
;; The hooks added are as follows:
;;
;; `elfeed-barb-before-show-functions':
;;
;;    This hook is run just before an entry is shown.  Each function
;;    is called in turn, with the entry to be run as an argument,
;;    until one returns non-nil.  This can be used to intercept
;;    entries matching specific criteria and display them in another
;;    way (such as in a web browser).
;;
;; To enable these hooks, just require `elfeed-barb'.

;;; Code:

(require 'elfeed)


;;;; Hook Variables
;;;;===============

(defcustom elfeed-barb-before-show-functions nil
  "Abnormal hook run before showing an entry in Elfeed.

Each hook function is passed a single argument, the Elfeed entry
that is about to be shown.  If any function returns non-nil, then
the entry is not shown and further functions are not
called."
  :group 'elfeed
  :type 'hook)


;;;; Hook Insertion
;;;;===============

(defun elfeed-barb--before-show-functions-advice (entry)
  "Advice to enable `elfeed-barb-before-show-functions'.
Intended as :before-until advice for `elfeed-show-entry'."
  (require 'validate)
  (validate-variable 'elfeed-barb-before-show-functions)
  (run-hook-with-args-until-success
   'elfeed-barb-before-show-functions entry))

(advice-add 'elfeed-show-entry
            :before-until #'elfeed-barb--before-show-functions-advice)


;;;; Unloading
;;;;==========

(defun elfeed-barb-unload-function ()
  "Remove all hooks added by `elfeed-barb' module."
  (advice-remove 'elfeed-show-entry
                 #'elfeed-barb--before-show-functions-advice))

(provide 'elfeed-barb)
;;; elfeed-barb.el ends here
