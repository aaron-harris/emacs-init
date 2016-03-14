;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; WHICH-FUNCTION-MODE EXTENSIONS
;;;;============================================================================

;; Functions extending module `which-func'
(require 'which-func)

(require 'aph-subr)                     ; For `aph/assoc-delete-in'


;;; Header Line Display
;;;====================
;; This code is adapted from a snippet I found here:
;;   https://www.emacswiki.org/emacs/WhichFuncMode

(defvar aph/which-func-header-line-format
  '(which-func-mode ("" which-func-format))
  "Header line format construct for `which-function-mode'.
Not enabled until `aph/which-func-use-header-line' has been
called.")

(defun aph/which-func-use-header-line-advice ()
  "Advice making `which-function-mode' use the header line.
Applied by `aph/which-func-use-header-line'.
Intended as :after advice for `which-func-ff-hook'."
  (when which-func-mode 
    (aph/assoc-delete-in mode-line-misc-info 'which-func-mode)
    (setq header-line-format aph/which-func-header-line-format)))

(defun aph/which-func-use-header-line ()
  "Move `which-function-mode' info to the header line." 
  (aph/which-func-use-header-line-advice)
  (advice-add 'which-func-ff-hook :after #'aph/which-func-use-header-line-advice))


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
