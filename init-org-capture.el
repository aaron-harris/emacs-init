;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM CAPTURE TEMPLATES
;;;;============================================================================

(require 'dash)                         ; For ->
(require 'aph-org)


;;; Capture Template Definitions
;;;=============================
(setq org-capture-templates
      `(("n" "Note" entry
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

        ("p" "Plain text" plain
         (function aph/org-capture-choose-target)
         "%i%?"
         :empty-lines 1
         :kill-buffer)

        ("g" "Grocery list item" item
         (file+headline ,(concat org-directory "/shopping.org") "Grocery List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)

        ("s" "Shopping list item" item
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

        ("c" "Comic Book" entry
         (file+headline ,(concat org-directory "/media.org") "Unfiled Comics")
         ,(aph/org-capture-add-properties
           "* UNREAD [[%^{Link to comic|%x}][%\\2 #%\\3]]%?"
           '(("Series"    . "%^{Series}")
             ("Issue_No"  . "%^{Issue number}")
             ("Writer"    . "%^{Writer}")
             ("Published" . "%^{Date published}u")))
         :kill-buffer)

        ("m" "Music to Buy" entry
         (file+headline ,(concat org-directory "/media.org") "Unfiled Music")
         ,(aph/org-capture-add-properties
           "* BUY %\\1 - %\\2%?"
           '(("Artist" . "%^{Artist}")
             ("Album"  . "%^{Album}")
             ("Genre"  . "%^{Genre}"))))))

(provide 'init-org-capture)
