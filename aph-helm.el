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


;;;; Imenu
;;========
;;;###autoload
(defun aph/helm-semantic-or-imenu (arg)
  "As `helm-semantic-or-imenu', but always show all candidates.

If the symbol at point is a valid candidate, go ahead and select
it, but still show all the other candidates.

Also, never jump directly to the definition for symbol at
point (overriding `helm-imenu-execute-action-at-once-if-one'),
even if there's only one candidate in the buffer."
  (interactive "P")
  (vizier-helm-append-keyword :input "")
  (let ((helm-imenu-execute-action-at-once-if-one nil))
    (helm-semantic-or-imenu arg)))


;;;; Projectile
;;=============
;;;###autoload
(defun aph/helm-browse-project (arg)
  "As `helm-browse-project', but truncate lines."
  (interactive "P")
  (vizier-helm-append-keyword :truncate-lines t)
  (helm-browse-project arg))

;;;###autoload
(defun aph/helm-projectile (&optional arg)
  "As `helm-projectile', but truncate lines."
  (interactive "P")
  (vizier-helm-append-keyword :truncate-lines t)
  (helm-projectile arg))

;;;###autoload
(defun aph/helm-projectile-grep (&optional dir)
  "As `helm-projectile-grep', but suspend updates initially."
  (interactive)
  (vizier-helm-toggle-initial-updates)
  (helm-projectile-grep dir))

(provide 'aph-helm)
;;; aph-helm.el ends here
