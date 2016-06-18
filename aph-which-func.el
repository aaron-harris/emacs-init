;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; WHICH-FUNCTION-MODE EXTENSIONS
;;;;============================================================================

;; Functions extending module `which-func'


;;; Org Mode Support
;;;=================
(defun aph/which-function-org ()
  "Help `which-function' find the right Org headline.
For use in `which-func-functions'."
  (when (eq major-mode 'org-mode)
    (if (org-before-first-heading-p)
        "-----"
      (org-get-heading :no-tags :no-todo))))


(provide 'aph-which-func)
