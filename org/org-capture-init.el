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

(setq org-capture-templates
      `(("n" "Note" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %?")
         :kill-buffer)

        ("N" "Link" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %A%?")
         :kill-buffer)

        ("q" "Clip" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %A%?\n  %i")
         :kill-buffer)

        ("Q" "Clip (with Link)" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %A%?\n  %i")
         :kill-buffer)

        ("t" "Task" entry
         (file+headline org-default-notes-file "Tasks")
         ,(-> "* TODO %^{Effort}p%?"
              aph/org-capture-add-logbook
              aph/org-capture-add-properties)
         :kill-buffer)

        ("T" "Task (with Link)" entry
         (file+headline org-default-notes-file "Tasks")
         ,(->  "* TODO %^{Effort}p%?%A."
               aph/org-capture-add-logbook
               aph/org-capture-add-properties)
         :kill-buffer)

        ("p" "Project" entry
         (file+headline org-default-notes-file "Projects")
         ,(aph/org-capture-add-logbook "* START %?")
         :kill-buffer)

        ("P" "Project (with Link)" entry
         (file+headline org-default-notes-file "Projects")
         ,(aph/org-capture-add-logbook "* START %?%A.")
         :kill-buffer)

        ("g" "Grocery List Item" item
         (file+headline "~/org/shopping.org" "Grocery List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)

        ("s" "Shopping List Item" item
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
           "* UNREAD %^L%?%^{Issue_No}p%^{Writer}p"
           '(("Published" . "%^{PUBLISHED}u"))) 
         :kill-buffer)))
