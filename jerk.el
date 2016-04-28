;;; jerk.el --- Cross-application yanking            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience kill-ring

;; Dependencies: `morgue'

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

;; 

;;; Code:

(require 'morgue)


;;; MS Access
;;;==========
(defun jerk-access ()
  "Yank a kill from MS Access, cleaning it up in the process.

More specifically, remove the first line of the most recent kill,
place the result back on the kill ring (leaving the original
in-place), then yank.

This function is intended to handle kills originating from MS
Access datasheets.  Such kills include a header row containing
the field names, which this function removes."
  (interactive)
  (morgue-yank
   (morgue-zap "\n")))


(provide 'jerk)
;;; jerk.el ends here
