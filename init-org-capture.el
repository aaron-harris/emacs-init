;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM CAPTURE TEMPLATES
;;;;============================================================================

(require 'dash)                         ; For ->
(require 'aph-org)


;;; Capture Template Definitions
;;;=============================
(setq org-capture-templates
      `(("n" "Note")
        ("np" "Plain note" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %?")
         :kill-buffer)
        ("nl" "Link" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %^L%?")
         :kill-buffer)
        ("nL" "Link to here" entry
         (file+headline org-default-notes-file "Notes")
         ,(aph/org-capture-add-logbook "* %A%?")
         :kill-buffer)

        ("t" "Task")
        ("tp" "Plain task" entry
         (file+headline org-default-notes-file "Tasks")
         ,(-> "* START %^{Effort}p%?"
              aph/org-capture-add-logbook
              aph/org-capture-add-properties)
         :kill-buffer)
        ("tl" "Link" entry
         (file+headline org-default-notes-file "Tasks")
         ,(-> "* START %^L%^{Effort}p%?"
              aph/org-capture-add-logbook
              aph/org-capture-add-properties)
         :kill-buffer)
        ("tL" "Link to here" entry
         (file+headline org-default-notes-file "Tasks")
         ,(-> "* START %A%^{Effort}p%?"
              aph/org-capture-add-logbook
              aph/org-capture-add-properties)
         :kill-buffer)

        ("p" "Plain text" plain
         (function aph/org-capture-choose-target)
         "%i%?"
         :empty-lines 1
         :kill-buffer)

        ("s" "Shopping list item")
        ("sg" "Grocery list item" item
         (file+headline ,(concat org-directory "/shopping.org") "Grocery List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)
        ("sm" "Generic shopping list item" item
         (file+headline ,(concat org-directory "/shopping.org") "Shopping List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)

        ("w" "Password" table-line
         (file ,(concat org-directory "/passwords.org"))
         ,(concat "| %^{Service} "
                  "| %^{Username|meerwolf@gmail.com} "
                  "| %^{Length} "
                  "| %^{Comments|epo}")
         :immediate-finish
         :unnarrowed
         :kill-buffer)

        ("m" "Media")
        ("mb" "Novel" entry
         (file+headline ,(concat org-directory "/media.org") "Unfiled Novels")
         ,(aph/org-capture-add-properties
           "* CONSUME %\\2%?"
           '(("Author"    . "%^{Author}")
             ("Title"     . "%^{Title}")
             ("Series"    . "%^{Series}")
             ("Series_No" . "%^{Number in series}")))
         :kill-buffer)
        ("mc" "Comic book" entry
         (file+headline ,(concat org-directory "/media.org") "Unfiled Comics")
         ,(aph/org-capture-add-properties
           "* CONSUME [[%^{Link to comic|%x}][%\\2 #%\\3]]%?"
           '(("Series"    . "%^{Series}")
             ("Issue_No"  . "%^{Issue number}")
             ("Writer"    . "%^{Writer}")
             ("Published" . "%^{Date published}u")))
         :kill-buffer)
        ("mt" "Television series" entry
         (file+headline ,(concat org-directory "/media.org") "Unfiled Television")
         ,(aph/org-capture-add-properties
           "* CONSUME %\\1: Season \\2%?"
           '(("Series"    . "%^{Series}")
             ("Season_No" . "%^{Season number}")))
         :kill-buffer)
        ("mn" "Movie (Netflix)" entry
         (file+headline ,(concat org-directory "/media.org") "Unfiled Television")
         ,(aph/org-capture-add-properties
           "* CONSUME %\\1%?"
           '(("Title"     . "%^{Title}")
             ("Series"    . "%^{Series}")
             ("Series_No" . "%^{Number in series}")))
         :kill-buffer)
        ("mm" "Music to buy" entry
         (file+headline ,(concat org-directory "/media.org") "Unfiled Music")
         ,(aph/org-capture-add-properties
           "* BUY %\\1 - %\\2%?"
           '(("Artist" . "%^{Artist}")
             ("Album"  . "%^{Album}")
             ("Genre"  . "%^{Genre}")))) 
        ("ml" "Internet link" entry
         (file+headline ,(concat org-directory "/media.org") "Internet")
         "* CONSUME %^L%?"
         :kill-buffer)))

(provide 'init-org-capture)
