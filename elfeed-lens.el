;;; elfeed-lens.el --- Multiple default filters -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience rss

;; Dependencies: `aph-seq', `validate' (optional)

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

;; This module adds support for multiple default filters in Elfeed.
;; To use it, just configure `elfeed-lens-list' (this should be a list
;; of the filters you want to use), then call `elfeed-lens-cycle' to
;; cycle through them.
;;
;; No special consideration is made for the default filter, so if you
;; want `elfeed-lens-cycle' to pass through it, you should include it
;; as an element of `elfeed-lens-list'.

;;; Code:

(defcustom elfeed-lens-list nil
  "A list of commonly-used filters for Elfeed.
Use `elfeed-lens-cycle' to cycle through these."
  :group 'elfeed
  :type '(repeat 'string))

;;;###autoload
(defun elfeed-lens-cycle (&optional verbose)
  "Apply the next filter in `elfeed-lens-list'.

If the current search filter is an element of `elfeed-lens-list',
apply the filter immediately following that one in the list,
looping back to the beginning if necessary.

If the current search filter is not in `elfeed-lens-list', just
apply the first filter in the list.

If `elfeed-lens-list' is empty, just apply the default filter.

Return the filter applied.  When called interactively or the
optional VERBOSE parameter is non-nil, also print a message
informing the user of the newly applied filter."
  (interactive "p")
  (require 'aph-seq)                    ; For `aph/seq-successor'
  (when (require 'validate nil :noerror)
    (validate-variable 'elfeed-lens-list))
  (let ((new-filter (or (aph/seq-successor elfeed-lens-list
                                           elfeed-search-filter
                                           #'equal :circular)
                        (car elfeed-lens-list))))
    (elfeed-search-set-filter new-filter)
    (when verbose (message "Filter applied: %s" elfeed-search-filter))
    elfeed-search-filter))

(provide 'elfeed-lens)
;;; elfeed-lens.el ends here
