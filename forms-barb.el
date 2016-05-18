;;; forms-barb.el --- More hooks for `forms-mode'    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data

;; Dependencies: `forms', `validate' (optional)
;; Advised functions from other packages:
;;   forms: `forms-jump-record'

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

;; This module adds more hooks for use in `forms-mode'.  Note that
;; these are injected using advice, contrary to the usual admonition
;; against advising other packages' functions.
;;
;; The hooks added are as follows:
;;
;; `forms-barb-change-record-hook':
;;
;;     This hook is run whenever the current record changes.
;;
;; To enable these hooks, just require `forms-barb'.

;;; Code:

(require 'forms)


;;;; Hook Variables
;;;;===============
(defcustom forms-barb-change-record-hook nil
  "Hook run after changing records in `forms-mode'."
  :group 'forms
  :type 'hook)


;;;; Hook Insertion
;;;;===============
(defun forms-barb--change-record-hook-advice (&rest args)
  "Run `forms-barb-change-record-hook'.

The ARGS are ignored.  The reason for including them is so that
this function can be used as advice.  By default this function is
installed as :after advice on `forms-jump-record'."
  (when (require 'validate nil :noerror)
    (validate-variable 'forms-barb-change-record-hook))
  (run-hooks 'aph/forms-change-record-hook))

(advice-add 'forms-jump-record :after #'forms-barb--change-record-hook-advice)


;;;; Unloading
;;;;==========
(defun forms-barb-unload-function ()
  "Remove all hooks added by `forms-barb' module."
  (advice-remove 'forms-jump-record #'forms-barb--change-record-hook-advice)) 

(provide 'forms-barb)
;;; forms-barb.el ends here
