;;; cygwinize.el --- Extra compat utils for Cygwin -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: os w32 cygwin

;; Dependencies: `vizier'

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

;; This module contains some extra compatability utilities that are
;; useful for interacting with Cygwin, and with Windows in general.

;;; Code:

(require 'vizier)


;;;; Filename Compatability
;;=========================
(defun cygwinize-convert-file-name-to-hybrid-windows
    (file &optional absolute-p)
  "As `cygwin-convert-file-name-to-windows', with forward slashes."
  (replace-regexp-in-string "\\\\" "/"
                            (cygwin-convert-file-name-to-windows
                             file absolute-p)))

(defun cygwinize--advice (orig-fn &rest args)
  "Generic advice to \"cygwinize\" a function.

Within the advised function, calls to `buffer-file-name' return
Windows-style filenames (but with forward slashes, as in
`cygwinize-convert-file-name-to-hybrid-windows').  This may be
used to provide a compatability layer when Emacs is dealing with
processes from outside the Cygwin ecosystem.

This advice should be installed as :around advice, and it is safe
to do so even on non-Cygwin systems, as the advice checks
`system-type' before proceeding."
  (vizier-with-advice-if (eq system-type 'cygwin)
      ((buffer-file-name
        :filter-return
        (lambda (return)
          (when return
            (require 'cygwinize)
            (cygwinize-convert-file-name-to-hybrid-windows return)))))
    (apply orig-fn args)))

(defun cygwinize (symbol)
  "Install advice to Cygwinize SYMBOL (a function name).

In all subsequent calls to the function named by SYMBOL, the
function `buffer-file-name' will return Windows-style
filenames (but with forward slshes, as in
`cygwinize-convert-file-name-to-hybrid-windows').  This may be
used to provide a compatability layer when Emacs is dealing with
processes from outside the Cygwin ecosystem.

If the current system is not Cygwin, do nothing."
  (when (eq system-type 'cygwin)
    (advice-add symbol :around #'cygwinize--advice)))

(defun cygwinize-remove (symbol)
  "Remove advice installed on SYMBOL with `cygwinize'."
  (advice-remove symbol #'cygwinize--advice))


;;;; DOS line endings
;;===================

;; From Johan Bockgård via
;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun cygwinize-hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(provide 'cygwinize)
;;; cygwinize.el ends here
