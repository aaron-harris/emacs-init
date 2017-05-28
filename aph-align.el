;;; aph-align.el --- Extensions for `align` module   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Required features: `align'

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

;; Miscellaneous functions extending those in the `align' module built
;; into Emacs.

;;; Code:
(require 'align)


;;; Basic Extensions
(defun aph/align (beg end &optional separate rules exclude-rules)
  "As `align', but always use prefix argument."
  (interactive "r")
  (let ((current-prefix-arg '(4)))
    (align beg end separate rules exclude-rules)))


(provide 'aph-align)
;;; aph-align.el ends here 
