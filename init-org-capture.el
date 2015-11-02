;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM CAPTURE TEMPLATES
;;;;============================================================================

(require 'cl-lib)                       ; For `cl-defun'
(require 'dash)                         ; For `->'


;;; Subroutines
;;;============
;; This section contains functions that are useful when defining
;; capture templates.

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


;;; Capture Template Definitions
;;;=============================
(setq org-capture-templates nil)

(add-to-list 'org-capture-templates '("n" "Note"))

(add-to-list 'org-capture-templates
             `("np" "Plain note" entry
              (file+headline org-default-notes-file "Notes")
              ,(aph/org-capture-add-logbook "* %?")
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("nl" "Link" entry
              (file+headline org-default-notes-file "Notes")
              ,(aph/org-capture-add-logbook "* %^L%?")
              :kill-buffer))

(add-to-list 'org-capture-templates
             `("nL" "Link to here" entry
              (file+headline org-default-notes-file "Notes")
              ,(aph/org-capture-add-logbook "* %A%?")
              :kill-buffer))

(add-to-list 'org-capture-templates '("t" "Task"))

(add-to-list 'org-capture-templates
             `("tp" "Plain task" entry
               (file+headline org-default-notes-file "Tasks")
               ,(-> "* TODO %^{Effort}p%?"
                    aph/org-capture-add-logbook
                    aph/org-capture-add-properties)
               :kill-buffer))

(add-to-list 'org-capture-templates
             `("tl" "Link" entry
               (file+headline org-default-notes-file "Tasks")
               ,(-> "* TODO %^L%^{Effort}p%?"
                    aph/org-capture-add-logbook
                    aph/org-capture-add-properties)
               :kill-buffer))

(add-to-list 'org-capture-templates
             `("tL" "Link to here" entry
              (file+headline org-default-notes-file "Tasks")
              ,(-> "* TODO %A%^{Effort}p%?"
                   aph/org-capture-add-logbook
                   aph/org-capture-add-properties)
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
