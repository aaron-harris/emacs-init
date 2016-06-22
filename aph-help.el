;;; aph-help.el --- Extensions for `help' module     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris;;; -*- lexical-binding: t -*- <meerwolf@gmail.com>
;; Keywords: help

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

;; Miscellaneous functions extending those in the `help' module built
;; into Emacs.

;;; Code:

;; Extensions for `help' module.
(require 'help)


;;;; No-Confirmation Revert
;;=========================
(defun aph/help-mode-revert-buffer (ignore-auto _noconfirm)
  "As `help-mode-revert-buffer', but skip confirmation.

To enable this feature, put this function in
`revert-buffer-function' in `help-mode'."
  (help-mode-revert-buffer ignore-auto :noconfirm))

(provide 'aph-help)
;;; aph-help.el ends here
