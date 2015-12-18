;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM CAPTURE TEMPLATES
;;;;============================================================================

(require 'dash)                         ; For `->'
(require 'aph-org-capture)


;;; Capture Template Definitions
;;;=============================
(setq org-capture-templates nil)

(add-to-list 'org-capture-templates '("n" "Note"))

(add-to-list 'org-capture-templates
             `("np" "Plain note" entry
              (file+headline org-default-notes-file "Notes")
              "* %?"
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("nl" "Link" entry
              (file+headline org-default-notes-file "Notes")
              "* %^L%?"
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("nL" "Link to here" entry
              (file+headline org-default-notes-file "Notes")
              "* %A%?"
              :kill-buffer))

(add-to-list 'org-capture-templates '("t" "Task"))

(add-to-list 'org-capture-templates
             `("tp" "Plain task" entry
               (file+headline org-default-notes-file "Tasks")
               "* TODO %?"
               :kill-buffer))

(add-to-list 'org-capture-templates
             `("tl" "Link" entry
               (file+headline org-default-notes-file "Tasks")
               "* TODO %^L%?"
               :kill-buffer))

(add-to-list 'org-capture-templates
             `("tL" "Link to here" entry
              (file+headline org-default-notes-file "Tasks")
              "* TODO %A%?"
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("p" "Plain text" plain
              (function aph/org-capture-choose-target)
              "%i%?"
              :empty-lines 1
              :kill-buffer))

(add-to-list 'org-capture-templates '("s" "Shopping list item"))

(add-to-list 'org-capture-templates
             `("sg" "Grocery list item" item
               (file+headline ,(concat org-directory "/shopping.org")
                              "Grocery List")
              "[ ] %?"
              :unnarrowed
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("sm" "Generic shopping list item" item
               (file+headline ,(concat org-directory "/shopping.org")
                              "Shopping List")
              "[ ] %?"
              :unnarrowed
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("w" "Password" table-line
              (file ,(concat org-directory "/passwords.org"))
              ,(concat "| %^{Service} "
                       "| %^{Username|meerwolf@gmail.com} "
                       "| %^{Length} "
                       "| %^{Comments|epo}")
              :immediate-finish
              :unnarrowed
              :kill-buffer))

(add-to-list 'org-capture-templates '("m" "Media"))

(add-to-list 'org-capture-templates
             `("mb" "Novel" entry
               (file+headline ,(concat org-directory "/media.org")
                              "Unfiled Novels")
              ,(aph/org-capture-add-properties
                "* MEDIA %\\2%?"
                '(("Author"    . "%^{Author}")
                  ("Title"     . "%^{Title}")
                  ("Series"    . "%^{Series}")
                  ("Series_No" . "%^{Number in series}")))
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("mc" "Comic book" entry
               (file+headline ,(concat org-directory "/media.org")
                              "Unfiled Comics")
              ,(aph/org-capture-add-properties
                "* MEDIA [[%^{Link to comic|%x}][%\\2 #%\\3]]%?"
                '(("Series"    . "%^{Series}")
                  ("Issue_No"  . "%^{Issue number}")
                  ("Writer"    . "%^{Writer}")
                  ("Published" . "%^{Date published}u")))
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("mt" "Television series" entry
               (file+headline ,(concat org-directory "/media.org")
                              "Unfiled Television")
              ,(aph/org-capture-add-properties
                "* MEDIA %\\1: Season \\2%?"
                '(("Series"    . "%^{Series}")
                  ("Season_No" . "%^{Season number}")))
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("mn" "Movie (Netflix)" entry
               (file+headline ,(concat org-directory "/media.org")
                              "Unfiled Television")
               ,(aph/org-capture-add-properties
                 "* MEDIA %\\1%?"
                 '(("Title"     . "%^{Title}")
                   ("Series"    . "%^{Series}")
                   ("Series_No" . "%^{Number in series}")))
               :kill-buffer))

(add-to-list 'org-capture-templates
             `("mm" "Music to buy" entry
               (file+headline ,(concat org-directory "/media.org")
                              "Unfiled Music")
              ,(aph/org-capture-add-properties
                "* BUY %\\1 - %\\2%?"
                '(("Artist" . "%^{Artist}")
                  ("Album"  . "%^{Album}")
                  ("Genre"  . "%^{Genre}"))))) 

(add-to-list 'org-capture-templates
             `("ml" "Internet link" entry
               (file+headline ,(concat org-directory "/media.org")
                              "Internet")
               "* MEDIA %^L%?"
              :kill-buffer))

(setq org-capture-templates (nreverse org-capture-templates))

(provide 'init-org-capture)
