;;; aph-window.el --- Extensions for `window' module -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `window'

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

;; Extensions for the `window' module built into Emacs.

;;; Code:

(require 'window)
(require 'hydra)


;;; Extensions to `other-window'
;;;=============================
(defun aph/other-window-backward (count &optional all-frames)
  "As `other-window' but reversed."
  (interactive "p")
  (other-window (- count) all-frames))


;;; Scrolling Commands
;;;===================
(defhydra aph/hydra-scroll-other (:color pink)
  "Scroll other"
  ("C-v" scroll-other-window      "fwd")
  ("M-v" (scroll-other-window '-) "back") 
  ("C-g" nil                      "quit" :color blue))

(provide 'aph-window)
;; aph-window.el ends here
