;;; aph-haskell.el --- Extensions for `haskell-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: languages haskell

;; Dependencies: `haskell-mode', `cygwinize' (Cygwin only),
;;   `vizier' (Cygwin only)

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

;; Miscellaneous functions extending those associated with
;; `haskell-mode'.

;;; Code:

(require 'haskell-mode)


;;; Indentation
;;;============
;; Functions in this section deal with indentation modes within
;; Haskell mode.
(defun aph/haskell-indentation-mode:off ()
  "Turn off `haskell-indentation-mode'.
This is equivalent to calling `haskell-indentation-mode' with an
argument of -1 and is intended for use in hooks (where using a
`lambda' can be problematic)."
  (haskell-indentation-mode -1))


;;; Compatability
;;;==============
;; Functions in this section implement compatability for my setup.
(defun aph/haskell-process-load-file-cygwin ()
  "As `haskell-process-load-file', with Cygwin compatibility.

This function detects a Cygwin system using the variable
`system-type'; non-Cygwin systems should be unaffected." 
  (interactive) 
  (if (not (eq system-type 'cygwin))
      (haskell-process-load-file)
    (require 'vizier)
    (require 'cygwinize)
    (vizier-with-advice
        ((#'buffer-file-name
          :filter-return #'cygwinize-convert-file-name-to-hybrid-windows))
      (haskell-process-load-file))))

(provide 'aph-haskell)
;;; aph-haskell.el ends here
