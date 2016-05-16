;;; browse-url-prefix.el --- Dynamic browser choice  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: hypermedia

;; Dependencies: `browse-url', `validate'

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

;; This module adds prefix argument support to `browse-url', so that a
;; different browser is used when you call `browse-url' with a prefix
;; argument.
;;
;; To use this module, you can just point the variable
;; `browse-url-browser-function' to `browse-url-prefix' and set up the
;; variables `browse-url-prefix-browser-function' and
;; `browse-url-prefix-default-browser-function' with the
;; `browse-url-*' functions corresponding to the browsers you wish to
;; use.  Then any call to `browse-url' will be handled by
;; `browse-url-prefix'.
;;
;; The setup described above will intercept *all* calls to
;; `browse-url'.  This means that a command that calls `browse-url'
;; directly but uses the prefix argument for something else may not
;; work as expected.  If this is problematic, you may wish to leave
;; `browse-url-browser-function' alone and just call
;; `browse-url-prefix' directly.
;;
;; If you wish to use the "URL dispatch" functionality of
;; `browse-url-browser-function' (where the browser is chosen based on
;; the URL selected), note that neither `browse-url-prefix-browser-function'
;; nor `browse-url-prefix-default-browser-function' supports this
;; directly, so URL dispatch will necessarily happen before
;; `browse-url-prefix' can interpret the prefix argument, and no URL
;; dispatch is available when calling `browse-url-prefix' directly.

;;; Code:

(require 'browse-url)
(require 'validate)


;;;; User Options
;;;;=============
(defgroup browse-url-prefix nil
  "Choose a web browser based on prefix arg."
  :prefix "browse-url-prefix-"
  :link '(emacs-commentary-link "browse-url-prefix")
  :group 'browse-url)

(defcustom browse-url-prefix-browser-function
  #'browse-url-default-browser
  "Function that `browse-url' should use with prefix arg.

This function should obey the same calling convention as
`browse-url-browser-function'; i.e., it should take a url as its
first argument."
  :type 'function)

(defcustom browse-url-prefix-default-browser-function
  #'browse-url-default-browser
  "Function that `browse-url' should use without prefix arg.

This function should obey the same calling convention as
`browse-url-browser-function'; i.e., it should take a url as its
first argument."
  :type 'function)


;;;; Dispatch Function
;;;;==================
;;;###autoload
(defun browse-url-prefix (url &rest args)
  "Browse URL using a browser determined by the prefix arg.

With a prefix arg, the function

Interactively, prompt the user for URL, using any URL at point as
a default."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply (if current-prefix-arg
             (validate-variable 'browse-url-prefix-browser-function)
           (validate-variable 'browse-url-prefix-default-browser-function))
         (cons url args)))


(provide 'browse-url-prefix)
;;; browse-url-prefix.el ends here
