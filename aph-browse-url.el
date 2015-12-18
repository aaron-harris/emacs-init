;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; WEB BROWSING FUNCTIONS
;;;;============================================================================

;;; This file contains extensions to the module `browse-url'

(defun aph/browse-url-prefer-eww (external url &rest args)
  "Browse URL in `eww', or in an external browser.

If a prefix argument is supplied, browse URL in an external
browser; otherwise, use `eww'.

Interactively, prompt the user for URL, using any URL at point as
a default."
  (interactive (cons current-prefix-arg
                     (browse-url-interactive-arg "URL: ")))
  (if (not external)
      (eww-browse-url url)
    (message "Sending URL to external browser.") 
    (apply #'browse-url (cons url args))))

(defun aph/browse-url-prefer-external (eww url &rest args)
  "Browse URL in an external browser, or in `eww'.

If a prefix argument is supplied, browse URL in `eww'; otherwise,
use an external browser.

Interactively, prompt the user for URL, using any URL at point as
a default." 
  (interactive (cons current-prefix-arg
                     (browse-url-interactive-arg "URL: ")))
  (apply #'aph/browse-url-prefer-eww (not eww) url args))


(provide 'aph-browse-url)