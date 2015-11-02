;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ELFEED FUNCTIONS
;;;;============================================================================

;;; This file contains functions extending `elfeed'.


;;; Link Tags
;;;==========
;; Functions in this section cause entries tagged with the 'link tag
;; to bypass the entry text and open the linked URL in a browser.

;;;###autoload
(defun aph/elfeed-search-show-entry (entry &optional external)
  "As `elfeed-search-show-entry', but intelligently follow links.

If ENTRY is tagged with the 'link tag, presume that the text of
ENTRY will be incomplete and open the url in ENTRY's link field.
If the optional parameter EXTERNAL is supplied (interactively, with a
prefix argument), use an external browser; otherwise, use `eww'.

If ENTRY doesn't have the \"link\" tag, call `elfeed-show-entry'.
In this case, ignore the EXTERNAL parameter."
  (interactive (list (elfeed-search-selected :ignore-region)
                     current-prefix-arg))
  (require 'elfeed)
  (require 'aph-advice)                 ; For `aph/advice-once'
  ;; We want to copy all behavior of `elfeed-search-show-entry',
  ;; except possibly the call to `elfeed-show-entry', which we
  ;; override using advice: 
  (when (elfeed-tagged-p 'link entry)
    (aph/advice-once
     #'elfeed-show-entry :override
     (lambda (entry) 
       (aph/browse-url-prefer-eww external (elfeed-entry-link entry)))))
  (elfeed-search-show-entry entry))

;;;###autoload
(defun aph/elfeed-show-next (&optional external)
  "As `elfeed-show-next', but intelligently follow links.
See `aph/elfeed-search-show-entry' for details."
  (interactive)
  (aph/advice-once #'elfeed-search-show-entry
                   :override #'aph/elfeed-search-show-entry) 
  (elfeed-show-next))

;;;###autoload
(defun aph/elfeed-show-prev (&optional external)
  "As `elfeed-show-prev', but intelligently follow links.
See `aph/elfeed-search-show-entry' for details."
  (interactive)
  (aph/advice-once #'elfeed-search-show-entry
                   :override #'aph/elfeed-search-show-entry) 
  (elfeed-show-prev))


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
  (let ((new-filter
         (aph/successor-in-list aph/elfeed-favorite-filters
                                elfeed-search-filter :cyclical)))
    (elfeed-search-set-filter new-filter)
    (when verbose (message "Filter applied: %s" elfeed-search-filter))
    elfeed-search-filter))

(provide 'aph-elfeed)
