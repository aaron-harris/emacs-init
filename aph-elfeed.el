;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ELFEED FUNCTIONS
;;;;============================================================================

;;; This file contains functions extending `elfeed'.
(require 'elfeed)


;;; Favorite Filters
;;;=================
(defvar aph/elfeed-favorite-filters nil
  "A list of commonly-used filters for Elfeed.
Use `aph/elfeed-search-next-favorite-filter' to cycle through
these.")

(defun aph/elfeed-search-next-favorite-filter (&optional verbose)
  "Apply the next filter in `aph/elfeed-favorite-filters'.

If the current search filter is an element of
`aph/elfeed-favorite-filters', apply the filter immediately
following that one in the list, looping back to the beginning if
necessary.

If the current search filter is not an element of
`aph/elfeed-favorite-filters', apply the first filter in the
list.

If `aph/elfeed-favorite-filters' is empty, just apply the default
filter.

Return the filter applied.  When called interactive or the
optional VERBOSE parameter is non-nil, also print a message
informing the user of the newly applied filter."
  (interactive "p")
  (require 'aph-dash)                   ; For `aph/successor-in-list'
  (let ((new-filter
         (aph/successor-in-list aph/elfeed-favorite-filters
                                elfeed-search-filter :cyclical)))
    (elfeed-search-set-filter new-filter)
    (when verbose (message "Filter applied: %s" elfeed-search-filter))
    elfeed-search-filter))

(provide 'aph-elfeed)
