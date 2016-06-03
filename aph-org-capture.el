;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG MODE CAPTURE EXTENSIONS
;;;;============================================================================

;; Extensions for `org-capture' module.
(require 'org-capture)
(require 'aph-advice)                   ; For `aph/with-advice'
(require 'cl-lib)                       ; For `cl-defun'


;;; Capture Template Builders
;;;==========================
(defun aph/org-capture-add-properties (template &optional props)
  "Append a property drawer containing PROPS to the capture TEMPLATE.

PROPS is an alist associating property names (strings) to their
desired values (also strings, which will typically include
template escapes like '%^').

If PROPS is omitted, the property drawer will be empty."
  (concat template
          "\n:PROPERTIES:"
          (mapconcat
           (lambda (x)
             (concat "\n:" (car x) ": " (cdr x)))
           props "")
          "\n:END:"))


;;; Capture Subroutines
;;;====================
;; This function needs to be a cl-defun because we need to distinguish between
;; the case where new-nodes is omitted and the case where it is supplied as nil.
;;
;; Most of this function's structure was taken from a Stackexchange answer by
;; user erikstokes.
(cl-defun aph/org-capture-choose-target
    (&optional (prompt "Capture at")
               (new-nodes org-refile-allow-creating-parent-nodes))
  "Prompt for a location in an Org-Mode file, then jump there.

This function is intended for use with the 'function option for
capture templates. If PROMPT is not supplied, it defaults to
\"Capture at\".

The optional parameter NEW-NODES will override the variable
`org-refile-allow-creating-parent-nodes' for the duration of this
command. If it is omitted, the default value of the variable will
be used."
  (let* ((target (save-excursion (org-refile-get-location
                                  prompt
                                  (not :default-buffer)
                                  new-nodes
                                  :include-current-subtree)))
         (file (nth 1 target))
         (pos (nth 3 target)))
    (find-file file)
    (goto-char pos)
    (org-end-of-subtree)
    (org-return)))

(provide 'aph-org-capture)
