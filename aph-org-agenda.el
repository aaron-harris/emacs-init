;;; aph-org-agenda.el --- Extensions for Org agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp

;; Dependencies: `org-agenda'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Commands extending the functionality of the Org-mode agenda.

;;; Code:

(require 'org-agenda)
(require 'aph-org)


;;;; Indentation Fixes
;;====================
(put 'org-agenda-with-point-at-orig-entry 'lisp-indent-function 1)


;;;; Timestamps
;;=============
(defun aph/org-agenda-date-later (arg)
  "As `org-agenda-date-later' but also work in TODO agendas.

Unlike `org-agenda-date-later', there is some question of which
timestamp to use.  If the entry contains a timestamp for today's
date, that timestamp is changed; otherwise, the first active
timestamp in the entry is changed.

Unlike `org-agenda-date-later', this command does not support
changing timestamps by increments other than days."
  ;; TODO: Make this undoable.
  (interactive "p")
  (if (org-agenda-check-type nil 'agenda 'timeline)
      (org-agenda-date-later arg)
    (let ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          stamp)
      (org-agenda-with-point-at-orig-entry nil 
        ;; First try today's date, then find any active timestamp.
        (unless (or (aph/org-find-timestamp nil 'active
                                            (aph/org-relative-timestamp))
                    (aph/org-find-timestamp nil 'active))
          (error "Could not find a timestamp to change"))
        (org-timestamp-up-day arg)
        (setq stamp (aph/org-timestamp-at-point)))
      (org-agenda-show-new-time marker stamp))))

(defun aph/org-agenda-date-earlier (arg)
  "As `org-agenda-date-earlier' but also work in TODO agendas.

Unlike `org-agenda-date-earlier', there is some question of which
timestamp to use.  If the entry contains a timestamp for today's
date, that timestamp is changed; otherwise, the first active
timestamp in the entry is changed.

Unlike `org-agenda-date-earlier', this command does not support
changing timestamps by increments other than days."
  (interactive "p")
  (aph/org-agenda-date-later (- arg)))

(provide 'aph-org-agenda)
;;; aph-org-agenda.el ends here
