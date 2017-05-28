;;; aph-haskell.el --- Extensions for `haskell-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: languages, haskell

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

;; Custom functions and commands for use with `haskell-mode'.

;;; Code:

(require 'haskell-compile)

(defcustom aph/haskell-test-command
  "stack test"
  "Command to use with `aph/haskell-test-project'."
  :type  'string
  :group 'haskell-compile)

(defun aph/haskell-test ()
  "As `haskell-compile', but run `aph/haskell-test-command'."
  (interactive)
  (let ((haskell-compile-cabal-build-command aph/haskell-test-command))
    (haskell-compile)))

(provide 'aph-haskell)
;;; aph-haskell.el ends here
