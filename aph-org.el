;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE
;;;;============================================================================

(require 'cl-lib)                   ; for `cl-defun'
(require 'dash)                     ; for `->>', `-find-index'
(require 'aph-functions)            ; for `aph/reductions',
                                    ;  `aph/weighted-comparator-maker'


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


;;; Capture
;;;========
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


;;; Refile
;;;=======
(defun aph/org-goto-last-refile ()
  "Goto last Org-mode item refiled.

This has the same effect as supplying a C-u C-u prefix argument
to `org-agenda-refile'.  It is intended for use globally, where a
keybinding for that function is not appropriate."
  (interactive)
  (org-agenda-refile '(16)))


;;; Agenda
;;;=======
(defun aph/org-agenda ()
  "As `org-agenda', and automatically refresh sticky agendas."
  (interactive)
  (when (equal (call-interactively #'org-agenda)
               "Sticky Agenda buffer, use `r' to refresh")
    (org-agenda-redo)))


;;; Agenda: Skip Functions
;;;=======================
;;; These are functions to be used with `org-agenda-skip-function'.
;;; The initial structure was taken from a stackexchange question, written by
;;; user Jonathan Leech-Pepin.

;; TODO: These functions are still a little quirky. Specifically, they only seem
;;       to work properly when the headline they're called on is visible. Until
;;       this is sorted out, all agenda files should have their default
;;       visibility setting set to CONTENTS or higher.
(defun aph/org-agenda-skip-tag (tag)
  "Skip current headline if it is tagged with TAG.

Return nil if headline containing point is tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines tagged with TAG (including by
inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                                  (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        (1- next-headline)
      nil)))

(defun aph/org-agenda-skip-without-tag (tag)
  "Skip current headline unless it is tagged with TAG.

Return nil if headline containing point is not tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines not tagged with TAG (including by
inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                             (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        nil
      (1- next-headline))))


;;; Agenda: Comparators
;;;====================
(defun aph/org-comparator-rating-to-weight-default (rating)
  "The default for `aph/org-comparator-rating-weight-function'.

This transformation is linear, mapping \[0,5\] onto
\[-0.125,0.125\]. Since larger values are typically interpreted
as better, use a descending sort to sort better values to the
top."
  (/ (- rating 2.5) 20.0))

(defvar aph/org-comparator-rating-to-weight
  #'aph/org-comparator-rating-to-weight-default
  "A function transforming a 0-5 rating to a coinflip bias.

Used with `aph/weighted-comparator-maker' in
`aph/org-rating-comparator'.

This function should take in a number in the range
\\[0,5] (representing a subjective \"5-star\" rating) and rescale
that number for use as a coinflip bias, as described in
`aph/weighted-comparator-maker'.")

(defvar aph/org-rating-property
  "Rating"
  "The name of the Org mode property used to store ratings.
Used in `aph/org-rating-comparator'.")

(defun aph/org-rating-comparator (x y &optional rating-prop)
  "Randomly return +1 or -1, biased by ratings of arguments.
Intended for use in a user-defined sorting strategy in an
Org-mode agenda (see `org-agenda-cmp-user-defined').

The arguments X and Y are Org-mode agenda entries, and the bias
on the coinflip is determined by the application of the function
in `aph/org-comparator-rating-to-weight' to the values X and Y
have for the property RATING-PROP (which is given by
`aph/org-rating-property' when omitted).

See `aph/weighted-comparator-maker' for more information."
  (let* ((rating-prop (or rating-prop aph/org-rating-property)) 
         (bias-fn
          (lambda (item)
            (--> (get-text-property 1 'org-marker item)
                 (org-entry-get it aph/org-rating-property)
                 (string-to-number it)
                 (funcall aph/org-comparator-rating-to-weight it)))))
    (funcall (aph/weighted-comparator-maker bias-fn) x y)))

(provide 'aph-org)
