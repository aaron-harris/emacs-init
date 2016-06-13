;;; org-match.el --- A better API for match strings  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions, lisp, org

;; Dependencies: `lexy', `org'

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

;; In my opinion, Org mode's interface for working with match strings
;; is inelegant and hard to work with (see, for instance,
;; `org-make-tags-matcher').  In particular, the user is required to
;; inject parameters using dynamic scoping, and this is difficult when
;; working in lexically-scoped codebases.
;;
;; This module aims to correct the deficiency by providing a more sane
;; interface to the underlying machinery.  Functions included are as
;; follows:
;;
;; `org-match-at-point-p':
;;
;;     This predicate is the "new interface" for match strings.  It
;;     takes a match string and returns non-nil exactly when the
;;     headline at point matches it.  With an optional argument,
;;     matches are restricted to TODO items.
;;
;; `org-match-skip':
;;
;;     This is a function that wraps `org-match-at-point-p' in the
;;     necessary logic to be used in `org-agenda-skip-function'.  By
;;     default, it will skip all headlines matching the specified
;;     match strings; with an optional argument, it will instead skip
;;     those headlines that do not match.

;;; Code:

(require 'lexy)
(require 'org)


;;;; Interface
;;============
(define-dynamically org-match-at-point-p (match &optional todo-only)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'.

If the optional argument TODO-ONLY is non-nil, do not declare a
match unless headline at point is a todo item."
  (let ((todo      (org-get-todo-state))
        (tags-list (org-get-tags-at)))
    (eval (cdr (org-make-tags-matcher match)))))


;;;; Agenda Matching
;;===================
(defun org-match-skip (match &optional keep)
  "Skip current headline if it matches MATCH.

If current headline matches MATCH (a match string of the same
format used by `org-tags-view'), return the position of the next
headline in current buffer.  Otherwise, return nil.  When used
with `org-agenda-skip-function', this will skip exactly those
headlines matching MATCH.

If the optional argument KEEP is non-nil, instead skip headlines
that do not match MATCH."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (when (org-xor (org-match-at-point-p match) keep)
      (save-excursion
        (or (outline-next-heading) (point-max))))))

(provide 'org-match)
;;; org-match.el ends here
