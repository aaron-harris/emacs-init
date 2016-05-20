;;; aph-helm.el --- Extensions for Helm              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris;;; -*- lexical-binding: t -*- <meerwolf@gmail.com>
;; Keywords: convenience helm

;; Dependencies: `vizier-helm'

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

;; Miscellaneous functions extending those from the `helm' package.

;;; Code:

(require 'vizier-helm)


;;;; Helm Commands
;;================
;;;###autoload
(defun aph/helm-browse-project (arg)
  "As `helm-browse-project', but truncate lines."
  (interactive "P")
  (vizier-helm-append-keyword :truncate-lines t)
  (helm-browse-project arg))

(provide 'aph-helm)
;;; aph-helm.el ends here
