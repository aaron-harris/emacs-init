;;; aph-page.el --- Extensions for `page' module     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `page', `hydra'

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

;; Extensions for the `page' module built into Emacs.

;;; Code:

(require 'page)
(require 'hydra)


;;; Page Motion Commands
;;;=====================
(defhydra aph/hydra-page (:color red)
  "Page motion"
  ("]"        forward-page  "forward")
  ("["        backward-page "back")
  ("<return>" nil           "quit" :color blue))

(provide 'aph-page)
;;; aph-page.el ends here
