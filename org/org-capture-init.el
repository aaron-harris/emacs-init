;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM CAPTURE TEMPLATES
;;;;============================================================================

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

This function is intended for use with the `function' option for
capture templates. If PROMPT is not supplied, it defaults to
\"Capture at\".

The optional parameter NEW-NODES will override the variable
`org-refile-allow-creating-parent-nodes' for the duration of this
command. If it is omitted, the default value of the variable will
be used."
  (let* ((target (save-excursion (org-refile-get-location
                                  "Capture at"
                                  (not :default-buffer)
                                  new-nodes
                                  :include-current-subtree)))
         (file (nth 1 target))
         (pos (nth 3 target)))
    (find-file file)
    (goto-char pos)
    (org-end-of-subtree)
    (org-return)))

(setq org-capture-templates
      `(("p" "Plain" plain
         (function aph/org-capture-choose-target)
         "%i%?"
         :empty-lines 1
         :kill-buffer)

        ("n" "Note" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %?")
         :kill-buffer)

        ("l" "Link" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %^L%?")
         :kill-buffer)

        ("L" "Link to here" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %A%?")
         :kill-buffer)

        ("t" "Task" entry
         (file+headline org-default-notes-file "Tasks")
         ,(-> "* START %^{Effort}p%?"
              aph/org-capture-add-logbook
              aph/org-capture-add-properties)
         :kill-buffer) 

        ("g" "Grocery list item" item
         (file+headline "~/org/shopping.org" "Grocery List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)

        ("s" "Shopping list item" item
         (file+headline "~/org/shopping.org" "Shopping List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)

        ("w" "Password" table-line
         (file "~/org/passwords.org")
         ,(concat "| %^{Service} "
                  "| %^{Username|meerwolf@gmail.com} "
                  "| %^{Length} "
                  "| %^{Comments|epo}")
         :immediate-finish
         :unnarrowed
         :kill-buffer)

        ("c" "Comic Book" entry
         (file+headline "~/org/media.org" "Unfiled Comics")
         ,(aph/org-capture-add-properties
           "* UNREAD [[%^{Link to comic|%x}][%\\2 #%\\3]]%?"
           '(("Series"    . "%^{Series}")
             ("Issue_No"  . "%^{Issue_No}")
             ("Writer"    . "%^{Writer}")
             ("Published" . "%^{PUBLISHED}u")))
         :kill-buffer)))
