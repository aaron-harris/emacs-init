;;; vizier-helm-test.el --- Tests for vizier-helm.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `vizier-helm', `ert', `proctor-helm'

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

;;; Code:

(require 'vizier-helm)

(require 'ert)
(require 'proctor-helm)


;;;; Macros
;;=========
;;; Since `vizier-with-helm' is the critical component of
;;; `proctor-with-helm' and we need the latter to write any decent
;;; tests of `vizier-with-helm', the tests for `proctor-with-helm' (in
;;; the module `proctor-helm-tests') should also suffice for testing
;;; `vizier-with-helm'.

(provide 'vizier-helm-test)
;;; vizier-helm-test.el ends here
