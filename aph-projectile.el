;;; aph-projectile.el --- Extensions for `projectile'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: project, convenience

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

;; Functions extending those found in the `projectile' package.

;;; Code:

(require 'projectile)


;;;; Miscellaneous
;;================
(defun aph/projectile-call-with-project (fun)
  "Call FUN with name of current project.
If not in a project, return nil."
  (and (projectile-project-p)
       (funcall fun (projectile-project-name))))


;;;; Test Prefixes and Suffixes
;;=============================
(defcustom aph/projectile-test-alist nil
  "An alist associating project types to test prefixes/suffixes.

This is used by the functions `aph/project-test-prefix' and
`aph/project-test-suffix' to override the hardcoded association
implemented by the functions `projectile-test-prefix' and
`projectile-test-suffix'.

Keys are symbols (project types), and values are pairs of
strings (PREFIX . SUFFIX).  Such an association indicates that
`aph/projectile-test-prefix' should return PREFIX for this
project type and `aph/projectile-test-suffix' should return
SUFFIX.  Either PREFIX or SUFFIX may be nil; this indicates that
the corresponding function will fall back to Projectile's default
implementation.")

(defun aph/projectile-test--prefix/suffix (project-type indexer fallback)
  "Subroutine used for `aph/projectile-test-*' functions.
These are `aph/projectile-test-prefix' and `aph/projectile-test-suffix'.

INDEXER is a function extracting the appropriate value from an
`assq' lookup in `aph/projectile-test-alist'.

FALLBACK is the apropriate `projectile-test-*' function to fall
back to."
  (or (funcall indexer (assq project-type aph/projectile-test-alist))
      (funcall fallback project-type)))

(defun aph/projectile-test-prefix (project-type)
  "As `projectile-test-prefix', with `aph/projectile-test-alist'.

The function `projectile-test-prefix' (the default implementation
for `projectile-test-prefix-function') hard-codes the association
between project types and test prefixes, making it difficult to
implement new project types or otherwise alter the association.

This function consults the variable `aph/projectile-test-alist'
first, and if no association is found, defers to
`projectile-test-prefix'."
  (aph/projectile-test--prefix/suffix
   project-type #'cadr #'projectile-test-prefix))

(defun aph/projectile-test-suffix (project-type)
  "As `projectile-test-suffix', with `aph/projectile-test-alist'.

The function `projectile-test-suffix' (the default implementation
for `projectile-test-suffix-function') hard-codes the association
between project types and test suffixes, making it difficult to
implement new project types or otherwise alter the association.

This function consults the variable `aph/projectile-test-alist'
first, and if no association is found, defers to
`projectile-test-suffix'."
  (aph/projectile-test--prefix/suffix
   project-type #'cddr #'projectile-test-suffix))

(provide 'aph-projectile)
;;; aph-projectile.el ends here
