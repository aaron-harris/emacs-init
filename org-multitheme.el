;;; org-multitheme.el --- Org support for multithemes -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: faces

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

;; When switching themes, `org-mode' buffers that use the "indent"
;; option need to be told to recalculate the color of the `org-hide'
;; face that makes leading stars invisible.  This module provides
;; support to do this automatically when changing themes using the
;; `multitheme-cycle' command.
;;
;; To use this module, just `require' it.  It is safe to do this even
;; if neither `org' nor `multitheme' has been loaded (although this
;; will do nothing until both modules have been loaded).

;;; Code:


;;;; Implementation
;;=================
(defun org-multitheme-update ()
  "Update definition of `org-hide' face to match current theme.

Run after changing themes to fix display problems with the
`org-hide' face."
  (when (featurep 'org)
    (let ((foreground (org-find-invisible-foreground)))
      (when foreground
	(set-face-foreground 'org-hide foreground)))))

(add-hook 'multitheme-base-theme-change-hook #'org-multitheme-update)


;;;; Unloading
;;============
(defun org-multitheme-unload-function ()
  "Undo changes made to Emacs by the `org-multitheme' module."
  (remove-hook 'multitheme-base-theme-change-hook #'org-multitheme-update))

(provide 'org-multitheme)
;;; org-multitheme.el ends here
