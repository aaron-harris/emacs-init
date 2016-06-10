;;; org-smart-agenda.el --- Context-sensitive agenda -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: calendar

;; Dependencies: `org-agenda', `validate' (optional)

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

;; This module defines the command `org-smart-agenda'.  This command
;; chooses an agenda to display according to the current date and
;; time.
;;
;; To configure this command, put the keys for the agenda views you
;; want `org-smart-agenda' to choose from in `org-smart-agenda-views'.
;; At present, there are four slots: weekday mornings, workday,
;; weekday evenings, and weekends.
;;
;; To configure when the "workday" begins and ends, configure the
;; variable `org-smart-agenda-workday'.  At present, there is no
;; option for customizing which days of the week are considered
;; workdays.

;;; Code:


;;;; User Options
;;===============
(defgroup org-smart-agenda nil
  "Automatically display the right agenda for the day and time."
  :prefix "org-smart-agenda-"
  :link   '(emacs-commentary-link "org-smart-agenda")
  :group  'org-agenda)

(defcustom org-smart-agenda-views '("m" "w" "e" "s")
  "The keys for the agenda views used by the smart agenda.
In order, the entries should be:
- The agenda to display on weekday mornings.
- The agenda to display during the workday.
- The agenda to display on weekday evenings.
- The agenda to display on weekends."
  :type '(list string string string string))

(defcustom org-smart-agenda-workday '(9 . 5)
  "The user's typical workday hours.

This should be a pair (START . END), where both elements are
hours according to a 24-hour clock.  These are used by
`org-smart-agenda' to determine when to display the work agenda."
  :type '(cons number number))


;;;; Commands
;;===========
;;;###autoload
(defun org-smart-agenda ()
  "Display an Org-mode agenda based on current day and time.

See the variables `org-smart-agenda-views' and
`org-smart-agenda-workday' for configuration options."
  (interactive)
  (when (require 'validate nil :noerror)
    (validate-variable 'org-smart-agenda-views)
    (validate-variable 'org-smart-agenda-workday))
  (let* ((start-work (car org-smart-agenda-workday))
         (end-work   (cdr org-smart-agenda-workday))
         (morning-ag (nth 0 org-smart-agenda-views))
         (work-ag    (nth 1 org-smart-agenda-views))
         (evening-ag (nth 2 org-smart-agenda-views))
         (weekend-ag (nth 3 org-smart-agenda-views)) 
         (day        (nth 6 (decode-time)))
         (hour       (nth 2 (decode-time)))
         (key        (cond
                      ((= day 0)            weekend-ag)
                      ((= day 6)            weekend-ag)
                      ((< hour start-work)  morning-ag)
                      ((>= hour end-work)   evening-ag)
                      (t                    work-ag))))
    (org-agenda nil key)))

(provide 'org-smart-agenda)
;;; org-smart-agenda.el ends here
