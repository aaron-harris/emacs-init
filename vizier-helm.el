;;; vizier-helm.el --- Advice for Helm               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions lisp tools advice helm

;; Dependencies: `vizier', `helm', `silence'

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

;; This module contains advice-like tools for modifying existing Helm
;; commands.  For the most part, these "advisors" should be called
;; immediately before the `helm' invocation you want to modify.
;;
;; Advisors included are as follows:
;;
;; `vizier-helm-append-keyword':
;;
;;     This is a general advisor that just adds a keyword argument to
;;     the next `helm' invocation.
;;
;; `vizier-helm-toggle-initial-updates':
;;
;;     This advisor makes the next `helm' invocation start as if the
;;     user had invoked `helm-toggle-suspend-update' immediately.  For
;;     most invocations, this will suspend updates, but if the command
;;     invoked specifies that updates should start suspended, then
;;     this will have the effect of resuming updates.  See also
;;     `vizier-helm-resume-update-or-exit-minibuffer', below.
;;
;; In addition, there are a few commands designed to work with these
;; advisors:
;;
;; `vizier-helm-resume-update-or-exit-minibuffer':
;;
;;     When using `vizier-helm-toggle-initial-updates', the user will
;;     need to call `helm-toggle-suspend-update' before any input can
;;     be processed by `helm'.  This can be quite a pain, so if you
;;     are using this advisor frequently, I recommend binding
;;     `vizier-helm-resume-update-or-exit-minibuffer' to <return> in
;;     `helm-map'.  If updates are suspended, this command will resume
;;     them; otherwise, it will exit `helm' like the usual <return>
;;     binding.

;;; Code:

(require 'vizier)
(require 'helm)


;;;; Advisor Functions
;;====================
(defun vizier-helm-append-keyword (keyword value)
  "Add KEYWORD and VALUE as args to next `helm' invocation."
  (vizier-advise-once
   #'helm :filter-args
   (lambda (args) 
     (append args (list keyword value)))))


;;;; Update Suspension
;;====================
(defun vizier-helm-toggle-initial-updates--hook-fn ()
  "Toggle updates; remove self from `helm-after-initialize-hook'.
Subroutine used by `vizier-helm-toggle-initial-updates'." 
  (unwind-protect
      (silence ("^Helm update suspended!$")
        (helm-toggle-suspend-update)) 
    (remove-hook 'helm-after-initialize-hook
                 #'vizier-helm-toggle-initial-updates--hook-fn)))

(defun vizier-helm-toggle-initial-updates ()
  "Toggle update suspend state for next `helm' invocation.

Cause the next `helm' invocation to start with updates suspended,
unless it specified that updates should start suspended, in which
case cause it to start with updates suspended."
  (add-hook 'helm-after-initialize-hook
            #'vizier-helm-toggle-initial-updates--hook-fn))

(defun vizier-helm-resume-update-or-exit-minibuffer ()
  "Resume updates if suspended, else `helm-maybe-exit-minibuffer'."
  (interactive) 
  (if helm-suspend-update-flag
      (silence ("^Helm update reenabled!$")
        (helm-toggle-suspend-update))
    (helm-maybe-exit-minibuffer)))

(provide 'vizier-helm)
;;; vizier-helm.el ends here
