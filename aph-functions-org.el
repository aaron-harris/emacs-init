;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE
;;;;============================================================================

(require 'org)

(require 'dash)                         ; for `->>', `-find-index'
(require 'aph-functions)                ; for `aph/reductions'


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

(require 'dash)                         ; for `->>'
(defun aph/org-sum-property-of-children
    (pom prop &optional inherit)
  "Return sum of PROP values for all children of heading at POM.

If INHERIT is non-nil, use inherited values for PROP. Ignore
non-numeric values."
  (->> (aph/org-get-property-of-children pom prop inherit)
       (mapcar #'string-to-number)
       (apply #'+)))


;;; Spinners
;;;=========
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

(defun aph/org-spin-weighted (&optional weight-prop)
  "As `aph/org-spin-basic', weighted by property WEIGHT-PROP.

The parameter WEIGHT-PROP should be the name of a property.
Non-negative numeric values for that property are treated as
weights for the spin. Non-numeric and negative values are treated
as zero.

When called interactively or if WEIGHT-PROP is
omitted,`aph/org-spin-weight-property' is used."
  (interactive)
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

(provide 'aph-functions-org)
