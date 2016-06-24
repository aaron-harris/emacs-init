;;; symbol.el --- Symbol manipulation library        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions

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

;; This module is intended as a symbol manipulation library, analogous
;; to the `s' library for strings.  That said, at present it contains
;; only one function, `symbol-concat'.  This function (which just adds
;; a string suffix to the given symbol) is dead simple and useful in a
;; wide variety of elisp code, but is unaccountably missing from core
;; Emacs.

;;; Code:
(defun symbol-concat (symbol suffix)
  "Return the symbol obtained from SYMBOL by appending SUFFIX.

The symbol returned is always the canonical symbol, even if
SYMBOL is uninterned.

SUFFIX should be a string."
  (intern (concat (symbol-name symbol) suffix)))

(provide 'symbol)
;;; symbol.el ends here
