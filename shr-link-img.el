;;; shr-link-img.el --- Customize image links in `shr'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: hypermedia

;; Dependencies: `shr', `vizier'
;; Advised functions from other packages:
;;   shr: `shr-urlify'

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

;; This module adds extra support for image links (that is, images
;; that are also links) to `shr'.  In particular, it adds a new face
;; `shr-link-img' which is used only for image links.  This face
;; inherits from `shr-link', but by default removes the underline
;; property; this is intended to fix display errors when using
;; `shr-zoom-image' on image links.

;;; Code:

(require 'shr)
(require 'vizier)


;;;; Faces
;;========
(defface shr-link-img
  '((t (:inherit shr-link :underline nil)))
  "Font for image links."
  :group 'shr)


;;;; Implementation
;;=================
(defun shr-link-img-advice (start url &optional title) 
  "Make `shr-urlify' use face `shr-link-img' for image links.
Intended as :before advice for `shr-urlify'."
  (when (get-text-property start 'image-url)
    (vizier-advise-once
     #'shr-add-font :filter-args
     (lambda (args)
       (setf (nth 2 args) 'shr-link-img)
       args))))

(advice-add 'shr-urlify :before #'shr-link-img-advice)


;;;; Unloading
;;============
(defun shr-link-img-unload-function ()
  "Undo changes made to Emacs by the module `shr-link-img'."
  (advice-remove 'shr-urlify #'shr-link-img-advice))

(provide 'shr-link-img)
;;; shr-link-img.el ends here
