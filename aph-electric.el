;;; aph-electric.el --- Electric extensions          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

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

;; Extensions for the `electric' module built into Emacs.

;;; Code:


;;;; Mode Switches
;;================
;;;###autoload
(defun aph/electric-indent-local-mode:off ()
  "Turn off `electric-indent-mode' only in this buffer."
  (interactive)
  (electric-indent-local-mode -1))

(provide 'aph-electric)
;;; aph-electric.el ends here
