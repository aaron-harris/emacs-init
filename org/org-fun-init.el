;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE FUNCTIONS
;;;;============================================================================

;; A function to skip tags in agenda view.  Modified from a function found on
;; stackexchange, written by user Jonathan Leech-Pepin.

;; This function (and aph/org-agenda-skip-without-tag, below) are still a little
;; quirky. Specifically, they only seem to work properly when the headline
;; they're called on is visible. Until I have this sorted out, all agenda files
;; should have their default visibility setting set to CONTENTS or higher.
(defun aph/org-agenda-skip-tag (tag)
  "Returns nil if headline containing point is tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with org-agenda-skip-function, where this will skip exactly
those headlines tagged with TAG (including by inheritance)."
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
  "Returns nil if headline containing point is not tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with org-agenda-skip-function, where this will skip exactly
those headlines not tagged with TAG (including by inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                             (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        nil
      (1- next-headline))))

;; Variables used to control aph/org-agenda-display-smart-agenda, below.
(setq aph/workday-start 10)
(setq aph/workday-end 18)

(defun aph/org-agenda-display-smart-agenda ()
  "Selects an Org-mode agenda to display, based on the current time and day of the week.

On Saturdays and Sundays, displays the weekend agenda. On
weekdays, displays the review agenda if the workday (as defined
by the variables aph/workday-start and aph/workday-end) hasn't
started yet, the work agenda if it's in progress, and the evening
agenda if it's already ended."
  (interactive)
  (let* ((day            (nth 6 (decode-time)))
         (hour           (nth 2 (decode-time))))
    (if (< 0 day 6)
        (cond
         ((< hour aph/workday-start) (org-agenda nil "r"))
         ((> hour aph/workday-end)   (org-agenda nil "2"))
         (t                          (org-agenda nil "1")))
      (org-agenda nil "3"))))
