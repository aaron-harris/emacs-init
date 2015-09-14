;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE
;;;;============================================================================

(require 'aph-functions)                ; For `aph/with-advice'
;; Some functions in this file also require the 'dash library at runtime.


;;; Heading Structure
;;;==================
(defun aph/org-count-children (&optional pom)
  "Return the number of children for Org mode heading at POM.

If POM (a number or a marker) is omitted, use point. If POM is
not in a heading, return nil."
  (let ((pom (or pom (point))))
    (save-excursion
      (goto-char pom)
      (unless (org-before-first-heading-p)
        (org-back-to-heading :invisible-ok)
        (if (org-goto-first-child)
            (let ((acc  1))
              (while (org-get-next-sibling)
                (setq acc (1+ acc)))
              acc)
          0)))))

(defun aph/org-goto-nth-child (n)
  "Goto the Nth child of heading at point.

Children are counted from 1. If heading does not have N children,
return nil and do not move point; otherwise, return point.

If N is zero, call `org-back-to-heading' and return point.

If N is negative, goto the (-N)th child from the end (so
(aph/org-goto-nth-child -1) moves to the last child)."
  (cond ((zerop n) (progn (org-back-to-heading)
                        (point))) 
        ((< n 0)   (aph/org-goto-nth-child
                  (+ (aph/org-count-children) (1+ n)))) 
        ((> n 0)
         (let ((target (catch 'fail
                         (save-excursion
                           (unless (org-goto-first-child)
                             (throw 'fail nil))
                           (dotimes (i (1- n) (point))
                             (unless (org-get-next-sibling)
                               (throw 'fail nil)))))))
           (when target
             (goto-char target))))))


;;; Properties
;;;===========
(defun aph/org-get-property-of-children
    (pom prop &optional inherit literal-nil)
  "Return list of PROP values for all children of heading at POM.
See `org-entry-get' for use of optional parameters."
  (save-excursion
    (goto-char pom)
    (unless (org-goto-first-child) '())
    (let ((acc (list (org-entry-get (point) prop inherit literal-nil))))
      (while (org-get-next-sibling)
        (push (org-entry-get (point) prop inherit literal-nil) acc))
      (nreverse acc))))

(defun aph/org-sum-property-of-children
    (pom prop &optional inherit)
  "Return sum of PROP values for all children of heading at POM.

If INHERIT is non-nil, use inherited values for PROP. Ignore
non-numeric values."
  (require 'dash)
  (->> (aph/org-get-property-of-children pom prop inherit)
       (mapcar #'string-to-number)
       (apply #'+)))


;;; Spinners
;;;=========

;;;###autoload
(defun aph/org-spin-basic ()
  "Move point to a random child of heading at point.
Return point."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Point not on heading.")
    (let ((die-size  (aph/org-count-children)))
      (aph/org-goto-nth-child (1+ (random die-size))))))

(defvar aph/org-spin-weight-property
  "Weight"
  "The default property to be used for `aph/org-spin-weight'.")

;;;###autoload
(defun aph/org-spin-weighted (&optional weight-prop)
  "As `aph/org-spin-basic', weighted by property WEIGHT-PROP.

The parameter WEIGHT-PROP should be the name of a property.
Non-negative numeric values for that property are treated as
weights for the spin. Non-numeric and negative values are treated
as zero.

When called interactively or if WEIGHT-PROP is
omitted,`aph/org-spin-weight-property' is used."
  (interactive)
  (require 'aph-functions) ; For `aph/reductions'
  (require 'dash)          ; For `->>', `-find-index'
  (if (org-before-first-heading-p)
      (message "Point not on heading.")
    (org-back-to-heading))
  (let* ((weight-prop  (or weight-prop aph/org-spin-weight-property))

         (weight-list
          (->> (aph/org-get-property-of-children (point) weight-prop)
               (mapcar #'string-to-number)
               (mapcar (lambda (x) (if (< x 0) 0 x)))))
         
         (threshold-list  (aph/reductions #'+ weight-list)) 
         (roll            (random (apply #'+ weight-list)))) 
    (->> threshold-list
         (-find-index (apply-partially #'< roll))
         1+
         aph/org-goto-nth-child)))


;;; Capture in New Frame
;;;=====================

;;;###autoload
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

;;;###autoload
(defun aph/org-capture-delete-capture-frame ()
  "Delete a frame named \"Capture\".
For use in `org-capture-after-finalize-hook' to clean up after
`aph/org-capture-in-popout-frame'."
  (require 'aph-framewin) ; For `aph/get-frame-by-name'
  (let ((frame  (aph/get-frame-by-name "Capture")))
    (when frame (delete-frame frame))))


;;; Refile
;;;=======

;;;###autoload
(defun aph/org-goto-last-refile ()
  "Goto last Org-mode item refiled.

This has the same effect as supplying a C-u C-u prefix argument
to `org-agenda-refile'.  It is intended for use globally, where a
keybinding for that function is not appropriate."
  (interactive)
  (org-agenda-refile '(16)))


;;; Agenda
;;;=======

;;;###autoload
(defun aph/org-agenda ()
  "As `org-agenda', and automatically refresh sticky agendas."
  (interactive)
  (when (equal (call-interactively #'org-agenda)
               "Sticky Agenda buffer, use `r' to refresh")
    (org-agenda-redo)))


;;; Links
;;;======

;;;###autoload
(defun aph/org-eww-store-link ()
      "Store the current eww url as an Org-Mode link."
      (when (eq major-mode 'eww-mode)
        (org-store-link-props
         :type         "http"
         :link         (eww-current-url)
         :description  (plist-get eww-data :title))))

(provide 'aph-org)
