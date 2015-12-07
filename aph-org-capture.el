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
(defun aph/org-capture-add-logbook (template)
  "Append a logbook drawer to the capture TEMPLATE.

The logbook drawer will contain a 'Captured' timestamp using the
capture escape '%U'."
  (concat template
          "\n:LOGBOOK:\n- Captured"
          (make-string 29 ? )           ; A string of 29 spaces.
          "%U\n:END:"))

(defun aph/org-capture-add-properties (template &optional props)
  "Append a property drawer containing PROPS to the capture TEMPLATE.

PROPS is an alist associating property names (strings) to their
desired values (also strings, which will typically include
template escapes like '%^').

If PROPS is omitted, the property drawer will be
empty. Explicitly including an empty drawer can be useful in the
situation where TEMPLATE already includes a logbook drawer;
otherwise, when properties are added to the entry during capture,
the resulting property drawer may be indented differently than
the logbook drawer."
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


;;; Capture in New Frame
;;;=====================
(defun aph/org-capture-in-popout-frame (&optional goto keys)
  "As `org-capture', but do all work in a new frame.

This function by itself doesn't clean up the frame following
capture.  To do that, add `aph/org-capture-delete-capture-frame'
to `org-capture-after-finalize-hook'."
  (interactive "P")
  (require 'aph-framewin)  ; For `aph/display-buffer-in-named-frame'
  (if goto
      (org-capture goto keys)
    (let ((override  '("\\*Org Select\\*\\|\\*Capture\\*\\|CAPTURE-.*"
                       aph/display-buffer-in-named-frame
                       (named-frame . "Capture"))))
      ;; Force all relevant buffers to open in a specific capture frame.
      (add-to-list 'display-buffer-alist override)
      (aph/with-advice 
          (;; Make Org-mode respect `display-buffer-alist'.
           (#'org-switch-to-buffer-other-window :override #'pop-to-buffer)
           ;; And stop Org-mode from messing with our window configuration.
           (#'delete-other-windows :override #'ignore))
        (unwind-protect (condition-case err
                            (org-capture goto keys)
                          (error (aph/org-capture-delete-capture-frame)
                                 (signal (car err) (cdr err))))
          (setq display-buffer-alist
                (delete override display-buffer-alist)))))))

(defun aph/org-capture-delete-capture-frame ()
  "Delete a frame named \"Capture\".
For use in `org-capture-after-finalize-hook' to clean up after
`aph/org-capture-in-popout-frame'."
  (require 'aph-framewin) ; For `aph/get-frame-by-name'
  (let ((frame  (aph/get-frame-by-name "Capture")))
    (when frame (delete-frame frame))))


(provide 'aph-org-capture)
