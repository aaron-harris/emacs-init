;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; WEB BROWSING FUNCTIONS
;;;;============================================================================

;;; This file contains functions interacting with web browsing in one
;;; way or another.


;;; Web Browsing
;;;=============
;; Functions in this section open web pages in a web browser, either
;; `eww' or a browser external to Emacs.

;;;###autoload
(defun aph/browse-url-prefer-eww (external url &rest args)
  "Browse URL in `eww', or in an external browser.

If a prefix argument is supplied, browse URL in an external
browser; otherwise, use `eww'.

Interactively, prompt the user for URL, using any URL at point as
a default."
  (interactive (cons current-prefix-arg
                     (browse-url-interactive-arg "URL: ")))
  (if external
      (message "Sending URL to external browser.") 
      (apply #'browse-url (cons url args))
    (eww-browse-url url)))

;;;###autoload
(defun aph/browse-url-prefer-external (eww url &rest args)
  "Browse URL in an external browser, or in `eww'.

If a prefix argument is supplied, browse URL in `eww'; otherwise,
use an external browser.

Interactively, prompt the user for URL, using any URL at point as
a default." 
  (interactive (cons current-prefix-arg
                     (browse-url-interactive-arg "URL: ")))
  (apply #'aph/browse-url-prefer-eww (not eww) url args))


;;; Elfeed
;;;=======
;; Functions in this section extend `elfeed'.

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

(provide 'aph-web)
