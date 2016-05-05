;;; cygwinize.el --- Extra compat utils for Cygwin -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: os w32 cygwin

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

(defun cygwinize-convert-file-name-to-hybrid-windows
    (file &optional absolute-p)
  "As `cygwin-convert-file-name-to-windows', with forward slashes."
  (replace-regexp-in-string "\\\\" "/"
                            (cygwin-convert-file-name-to-windows
                             file absolute-p)))

;; From Johan Bockgård via
;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun cygwinize-hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(provide 'cygwinize)
;;; cygwinize.el ends here
