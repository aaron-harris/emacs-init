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


;;; Capture in New Frame
;;;=====================
(defun aph/org-capture--closing-capture-frame (&optional goto keys)
  "As `org-capture', but delete capture frame on abort.
This is used as a subroutine by `aph/org-capture-in-whole-frame'
and `aph/org-capture-in-popout-frame'."
  (condition-case err
        (org-capture goto keys)
      (error (aph/org-capture-delete-capture-frame)
             (signal (car err) (cdr err)))))

(defun aph/org-capture-in-whole-frame (&optional goto keys)
  "As `org-capture', but take over entire frame.

This is designed to be used with the -e option for emacsclient,
where a frame has just been created that has no useful content in
it.  For normal usage, `aph/org-capture-in-popout-frame' probably
makes more sense.

To close the frame after capture, add
`aph/org-capture-delete-capture-frame' to
`org-capture-after-finalize-hook'."
  (interactive "P")
  (modify-frame-parameters nil '((name . "Capture")))
  (if goto (org-capture goto keys)
    (aph/with-advice
        (('org-switch-to-buffer-other-window
          :override
          #'pop-to-buffer-same-window))
      (aph/org-capture--closing-capture-frame goto keys))))

(defun aph/org-capture-in-popout-frame (&optional goto keys)
  "As `org-capture', but do all work in a new frame.

This function by itself doesn't clean up the frame following
capture.  To do that, add `aph/org-capture-delete-capture-frame'
to `org-capture-after-finalize-hook'."
  (interactive "P")
  (require 'aph-framewin)  ; For `aph/display-buffer-in-named-frame'
  (if goto (org-capture goto keys)
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
        (unwind-protect (aph/org-capture--closing-capture-frame goto keys)
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
