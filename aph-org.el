;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE
;;;;============================================================================

(require 'org)
(require 'aph-advice)             ; For `aph/with-advice'
(require 'aph-lexical)            ; For `aph/defun-dyn'
(require 'aph-dash)               ; For `->>', `aph/reductions'


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


;;; Match Strings
;;;==============
(aph/defun-dyn aph/org-match-at-point-p (match &optional todo-only)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'.

If the optional argument TODO-ONLY is non-nil, do not declare a
match unless headline at point is a todo item."
  (let ((todo      (org-get-todo-state))
        (tags-list (org-get-tags-at)))
    (eval (cdr (org-make-tags-matcher match)))))


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
(defun aph/org-agenda (&optional arg org-keys restriction)
  "As `org-agenda', and automatically refresh sticky agendas." 
  (interactive "P") 
  (when (equal (org-agenda arg org-keys restriction)
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


;;; Markup
;;;=======
(defconst aph/org-emphasis-alist-dict
  '(("*" bold           bold)
    ("/" italic         italic)
    ("_" underline      underline)
    ("+" strike-through (:strike-through t))
    ("~" code           org-code verbatim)
    ("=" verbatim       org-verbatim verbatim))
  "Alist mapping keys in `org-emphasis-alist' to desired markup.
Each entry should be a list (CHAR SYM . FACES), where CHAR is a
key in `org-emphasis-alist', SYM is the return value of
`org-element-text-markup-successor' associated with that
character, and FACES is the value associated with CHAR in
`org-emphasis-alist'.

This alist is essentially the logic inappropriately hard-coded by
`org-element-text-markup-successor', and is used by
`aph/org-emphasis-alist-update' to update that function's
behavior.")

(defun aph/org-emphasis-alist-update (old new)
  "Update `org-emphasis-alist' to use NEW char in place of OLD.

Because the function `org-element-text-markup-successor' uses
hard-coded logic assuming the default value of
`org-emphasis-alist', updating `org-emphasis-alist' is not enough
to change the character controlling a particular kind of
markup.  (It does suffice if you're just removing an entry,
however.)

This function makes the necessary changes to `org-emphasis-alist'
and also advises `org-element-text-markup-successor'
appropriately.  Because `org-emphasis-alist' recommends
restarting Org-mode after making changes to that variable, we do
that too.

Because this function reloads Org-mode, if you use it in a file
that is loaded automatically following Org-mode (e.g., in
the :config block of a `use-package' declaration), you should use
`require' make sure that the relevant feature is `provide'd
*before* this function is called to prevent the file from being
loaded twice.  This does not apply if you are using
`with-eval-after-load' instead of `use-package', as this macro
evaluates its body only once."
  (let ((sym  (or (cadr (assoc old aph/org-emphasis-alist-dict))
                  (error "Emphasis char %S not recognized" old)))
        (adv  (lambda (oldfn)
                (condition-case err
                    (funcall oldfn)
                  (error
                   (let ((delim
                          (->> (cadr err)
                               (replace-regexp-in-string "[^0-9]" "")
                               string-to-number
                               char-after)))
                     (if (= delim (elt 0 old))
                         (cons sym (match-beginning 2))
                       (signal (car err) (cdr err))))))))
        (name (intern (format "aph/org-emphasis-alist-update[%s]" new))))
    (setcar (assoc old org-emphasis-alist) new)
    (org-reload)
    (advice-add 'org-element-text-markup-successor :around
                adv `((name . ,name)))))


;;; Advice
;;;=======
(defun aph/org-todo-window-advice (orig-fn)
  "Advice to fix window placement in `org-fast-todo-selection'.
Intended as :around advice for `org-fact-todo-selection'."
  (require 'aph-framewin)      ; For `aph/display-buffer-in-subwindow'
  (let  ((override '("\\*Org todo\\*" aph/display-buffer-in-subwindow)))
    (add-to-list 'display-buffer-alist override)
    (aph/with-advice
        ((#'org-switch-to-buffer-other-window :override #'pop-to-buffer))
      (unwind-protect (funcall orig-fn)
        (setq display-buffer-alist
              (delete override display-buffer-alist)))))) 

(defun aph/org-cycle-smart-tab-advice (fn &optional arg)
  "Advice to make `org-cycle' use `smart-tab'.

With this advice :around `org-cycle', that function will use
`smart-tab' as its fallback action instead of just indenting.
All other behavior of `org-cycle' remains unchanged."
  (aph/with-advice
      ;; Make `org-cycle' use `smart-tab' as fallback action.
      ((#'global-key-binding :before-until
                             (lambda (keys &optional accept-default)
                               (when (equal keys "\t")
                                 #'smart-tab)))
       ;; Prevent `smart-tab' from using `org-cycle' as its fallback.
       (#'smart-tab-default :override #'indent-for-tab-command))
    (apply fn arg)))

(defun aph/org-update-faces ()
  "Update definition of `org-hide' to match current theme.
Run after changing themes to fix display problems with the
`org-hide' face."
  (let ((foreground (org-find-invisible-foreground)))
    (if foreground
        (set-face-foreground 'org-hide foreground))))

(provide 'aph-org)
