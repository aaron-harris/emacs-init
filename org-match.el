;;; org-match.el --- A better API for match strings  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions, lisp, org

;; Dependencies: `lexy'

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

;; In my opinion, Org mode's interface for working with match is
;; inelegant and hard to work with (see, for instance,
;; `org-make-tags-matcher').  In particular, the user is required to
;; inject parameters using dynamic scoping, and this is difficult when
;; working in lexically-scoped codebases.
;;
;; This module aims to correct the deficiency by providing a more sane
;; interface to the underlying machinery.  At present, this consists
;; of the sole function `org-match-at-point-p', which takes a match
;; string and returns non-nil exactly when the headline at point
;; matches it.  An option is also provided for restricting matches to
;; TODO items.

;;; Code:

(require 'lexy)

(define-dynamically org-match-at-point-p (match &optional todo-only)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'.

If the optional argument TODO-ONLY is non-nil, do not declare a
match unless headline at point is a todo item."
  (let ((todo      (org-get-todo-state))
        (tags-list (org-get-tags-at)))
    (eval (cdr (org-make-tags-matcher match)))))

(provide 'org-match)
;;; org-match.el ends here
