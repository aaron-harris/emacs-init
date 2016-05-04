;;; multitheme.el --- Use multiple themes together   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: themes

;; Dependencies: `dash'

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

;; Because custom themes are additive in Emacs (that is,
;; `enable-theme' doesn't disable any themes that were previously
;; enabled), it can be tricky to switch between different themes.  It
;; can be even trickier if you want certain themes to be used
;; simultaneously.  This module attempts to provide support for this
;; sort of use-case.
;;
;; To use this module, you should first set the variable
;; `multitheme-base-theme-list' to contain a list of themes (symbols)
;; that you'd like to use.  Then call the command `multitheme-cycle'
;; to enable to the first theme in the list; subsequent invocations
;; will cycle through the remaining themes.
;;
;; In addition, if you set the variable `multitheme-overtheme', its
;; value (again, a symbol naming a theme) will be applied on top of
;; whatever base theme is currently in use.

;;; Code:

(require 'dash)                         ; For `-drop-while'


;;; Configuration Variables
;;;========================
(defvar multitheme-base-theme-list nil
  "A list of themes to be used as multitheme bases.

Note that multitheme does not check whether these themes are
marked as \"safe\" and will load any theme without
confirmation.")

(defvar multitheme-overtheme nil
  "A theme used on top of the multitheme base.

If this variable is non-nil, it should be the name of a theme.
Whenever the multitheme base is changed (e.g., by
`multitheme-cycle'), this theme will be applied on top of the new
base theme.  Hence this theme should be \"thin\", overriding only
those elements that you wish to change in all base themes.

Note that multitheme does not check whether this theme is marked
as \"safe\" and will load any theme without confirmation.")

(defvar multitheme-base-theme-change-hook nil
  "Hook run when the multitheme base is changed.
When this hook is run, the new base has already been loaded.")


;;; Subroutines
;;;============
(defun multitheme--enable (theme)
  "As `enable-theme', but load the theme if necessary."
  (if (custom-theme-p theme)
      (enable-theme theme)
    (load-theme theme)))


;;; Commands
;;;=========

;;;###autoload
(defun multitheme-cycle ()
  "Cycle between the themes in `multitheme-base-theme-list'.
If none of these themes is currently active, instead enable the
first element of `multitheme-base-theme-list'.

Also re-enable `multitheme-overtheme' so it remains \"on top\" of
the base theme.

If a theme to be enabled is not yet defined, attempt to load it
first (using `load-theme').  Respect `custom-safe-themes'.

After all theme changes have been made, run
`multitheme-base-change-hook'."
  (interactive)
  (let ((themes (-drop-while
                 (lambda (thm) (not (custom-theme-enabled-p thm)))
                 multitheme-base-theme-list)))
    ;; Cycle base theme
    (if (null themes)
        (multitheme--enable (car multitheme-base-theme-list))
      (disable-theme (car themes))
      (multitheme--enable (or (cadr themes)
                              (car multitheme-base-theme-list))))
    ;; Reassert overtheme
    (when multitheme-overtheme
      (multitheme--enable multitheme-overtheme))
    ;; Run hooks
    (run-hooks 'multitheme-base-change-hook)))

(provide 'multitheme)
;;; multitheme.el ends here
