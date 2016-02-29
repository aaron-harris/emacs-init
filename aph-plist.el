;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; PLIST EXTENSIONS
;;;;============================================================================

;; Functions for interacting with plists.

(defun aph/plist-get-as-list (plist prop)
  "As `plist-get', but ensure result is a list.
If `plist-get' would return a list, return that list.  Otherwise,
return a list containing that value as its sole element."
  (let ((val (plist-get plist prop)))
    (if (listp val)
        val
      (list val))))


(provide 'aph-plist)
