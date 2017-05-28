;;; org-eww.el --- Support for Org links in `eww'    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: hypermedia

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

;; This module adds support to `eww' for Org mode links.  Since the
;; natural link type for `eww' already exists (http), the only support
;; necessary is telling `org-store-link' how to make a link from an
;; `eww' buffer.
;;
;; To use this module, just `require' it.  Don't worry; it's small.

;;; Code:


;;;; Implementation
;;=================
(defun org-eww-store-link ()
  "Store the current eww url as an Org-Mode link."
  (when (eq major-mode 'eww-mode)
    (org-store-link-props
     :type         "http"
     :link         eww-current-url
     :description  eww-current-title)))

(add-hook 'org-store-link-functions #'org-eww-store-link)


;;;; Unloading
;;============
(defun org-eww-unload-function ()
  "Reverse changes made to Emacs by org-eww.el."
  (remove-hook 'org-store-link-functions #'org-eww-store-link))

(provide 'org-eww)
;;; org-eww.el ends here
