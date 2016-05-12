;;; elfeed-link.el --- Skip stub entries in Elfeed   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience rss

;; Dependencies: `elfeed-barb'

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

;; Many RSS feeds contain entries that are incomplete or
;; uninteresting, hiding their content behind the link contained in
;; the entry.  This module provides elfeed with the ability to bypass
;; these entries and open the links directly in a web browser.
;;
;; To use this module, just require it and customize the variable
;; `elfeed-link-tag'.  This variable should contain a symbol which is
;; used as an Elfeed tag to denote entries whose text should be
;; bypassed.  You can then use the usual Elfeed facilities to tag all
;; entries from specific feeds with this tag.
;;
;; If you want to use a different browser to open Elfeed entries than
;; that normally used by `browse-url', put the appropriate function
;; (e.g., `browse-url-firefox' or `eww-browse-url') in the variable
;; `elfeed-link-browser-function'.

;;; Code:

(require 'elfeed-barb)


;;;; Customization Variables
;;;;========================

(defvar elfeed-link-tag nil
  "Elfeed tag whose entries should be opened as links.")

(defvar elfeed-link-browser-function nil
  "Function to use to open external links from Elfeed.

If nil, the default browser (`browse-url-browser-function') is
used.  Otherwise, this should be a function suitable for use as
`browse-url-browser-function'; that is, it should take any number
of arguments and expect a URL as its first argument.

This function is only used for links opened automatically on
entries with `elfeed-link-tag', not to links opened with
`elfeed-show-visit'.")


;;;; Implementation
;;;;===============
(defun elfeed-link-open (entry)
  "If ENTRY has `elfeed-link-tag', open it in a browser.

If `elfeed-link-browser-function' is non-nil, use that function
to open the URL rather than `browse-url-browser-function'.

Return non-nil if ENTRY has `elfeed-link-tag' and nil if it does
not.  This makes the function suitable for use in
`elfeed-barb-before-show-functions'."
  (when (and elfeed-link-tag
             (elfeed-tagged-p elfeed-link-tag entry))
    (let ((link  (elfeed-entry-link entry))
          (browse-url-browser-function
           (or elfeed-link-browser-function
               browse-url-browser-function)))
      (message "Opening entry link: %s" link)
      (browse-url link))))

(add-hook 'elfeed-barb-before-show-functions #'elfeed-link-open)


;;;; Unloading
;;;;==========
(defun elfeed-link-unload-function ()
  "Undo changes to Emacs made to support `elfeed-link' module."
  (remove-hook 'elfeed-barb-before-show-functions #'elfeed-link-open))

(provide 'elfeed-link)
;;; elfeed-link.el ends here
