;;; aph-theme.el --- Personal overtheme              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: themes

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

;; This module defines the `aph' theme, which I use as an overtheme
;; for the `multitheme' package.  That is, it contains face overrides
;; that I wish to apply regardless of which "actual theme" I am using.

;;; Code:

(deftheme aph "Personal theme of Aaron Harris")


;;; Theme Faces
;;;============
(custom-theme-set-faces
 'aph
 ;; Basic faces
 '(region ((t (:inverse-video t))))
 ;; Avy
 '(avy-lead-face-0 ((t (:inverse-video t))))
 ;; Font lock (decolorization)
 '(font-lock-builtin-face       ((t (:foreground nil :slant italic))))
 '(font-lock-comment-face       ((t (:slant italic))))
 '(font-lock-constant-face      ((t (:weight bold))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face       ((t (:foreground nil :weight ultra-bold))))
 '(font-lock-type-face          ((t (:foreground nil :underline t))))
 '(font-lock-variable-name-face ((t (:weight bold)))))


;;; Path Handling
;;;==============
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'aph)
;;; aph-theme.el ends here
