;;; org-barb.el --- More hooks for `org-mode'        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: calendar hypermedia wp org

;; Advised functions from other packages:
;;   org: `org-agenda-finalize'

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

;; This module adds more hooks for use in `org-mode'.  Note that these
;; are injected using advice, contrary to the usual admonition against
;; advising other packages' functions.
;;
;; The hooks added are as follows:
;;
;; `org-barb-agenda-pre-finalize-hook':
;;
;;     This hook is run just before an agenda being built is
;;     "finalized" with the function `org-agenda-finalize'.

;;; Code:


;;;; Hook Variables
;;=================
(defcustom org-barb-agenda-pre-finalize-hook nil
  "Hook run just before `org-agenda-finalize'."
  :group 'org-agenda
  :type  'hook)


;;;; Hook Insertion
;;=================
(defun org-barb--agenda-pre-finalize-hook-advice (&rest args)
  "Run `org-barb-agenda-pre-finalize-hook-advice'.
Ignore ARGS.

By default this function is installed as :before advice on
`org-agenda-finalize'."
  (when (require 'validate nil :noerror)
    (validate-variable 'org-barb-agenda-pre-finalize-hook))
  (run-hooks 'org-barb-agenda-pre-finalize-hook))

(advice-add 'org-agenda-finalize
            :before #'org-barb--agenda-pre-finalize-hook-advice)


;;;; Unloading
;;============
(defun orb-barb-unload-function ()
  "Remove all hooks added by `org-barb' module."
  (advice-remove 'org-agenda-finalize
                 #'org-barb--agenda-pre-finalize-hook-advice))

(provide 'org-barb)
;;; org-barb.el ends here
