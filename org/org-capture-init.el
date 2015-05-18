;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM CAPTURE TEMPLATES
;;;;============================================================================

(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline org-default-notes-file "Notes")
         "* %?
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("N" "Link" entry
         (file+headline org-default-notes-file "Notes")
         "* %A%?
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("q" "Clip" entry
         (file+headline org-default-notes-file "Notes")
         "* %A%?\n  %i
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("Q" "Clip (with Link)" entry
         (file+headline org-default-notes-file "Notes")
         "* %A%?\n  %i
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("t" "Task" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %^{Effort}p%?
:LOGBOOK:
- Captured                             %U
:END:
:PROPERTIES:
:END:"
         :kill-buffer)

        ("T" "Task (with Link)" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %^{Effort}p%?%A.
:LOGBOOK:
- Captured                             %U
:END:
:PROPERTIES:
:END:"
         :kill-buffer)

        ("p" "Project" entry
         (file+headline org-default-notes-file "Projects")
         "* START %?
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("P" "Project (with Link)" entry
         (file+headline org-default-notes-file "Projects")
         "* START %?%A.
:LOGBOOK:
- Captured                             %U
:END:"
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
         "| %^{Service} | %^{Username|meerwolf@gmail.com} | %^{Length} | %^{Comments|epo}"
         :immediate-finish
         :kill-buffer)

        ("c" "Comic Book" entry
         (file+headline "~/org/media.org" "Unfiled Comics")
         "* UNREAD %^L%?%^{Issue_No}p%^{Writer}p
:PROPERTIES:
:Published: %^{PUBLISHED}u
:END:"
         :kill-buffer)))
