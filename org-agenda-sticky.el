;;; org-agenda-sticky.el --- Extra support for sticky agendas  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: calendar

;; Advised functions from other packages:
;;   org-agenda: `org-agenda', `org-agenda-quit'

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

;; This module contains several features that modify how `org-mode'
;; handles sticky agendas (see the option `org-agenda-sticky'). Each
;; of these features can be disabled independently of one another,
;; using user options.
;;
;; Included features are as follows:
;;
;; * When displaying an agenda with `org-agenda', if a sticky agenda
;;   is already open, refresh it automatically.  This feature can be
;;   disabled with the option `org-agenda-sticky-auto-refresh'.
;;
;; In addition, there are some bugs in `org-mode' that directly affect
;; sticky agendas.  We patch these bugs using advice; unlike the
;; features listed above, the patches cannot be disabled by setting
;; user options.
;;
;; * There is a bug in the implementation of `org-agenda-quit' that
;;   can cause a sticky agenda to remain displayed (rather than being
;;   buried).  This bug applies when `org-agenda-window-setup' is set
;;   to the symbol `reorganize-frame'.

;;; Code:

(eval-when-compile (require 'vizier))


;;;; User Options
;;===============
(defcustom org-agenda-sticky-auto-refresh t
  "If non-nil, `org-agenda' will refresh sticky agendas.

Normally, if a sticky agenda is already open, calling
`org-agenda' will just bring that agenda to the front.  With this
option non-nil, the agenda will also be automatically
refreshed."
  :group 'org-agenda
  :type  'boolean)


;;;; Implementation
;;=================
(defun org-agenda-sticky-auto-refresh-advice (result)
  "Advice enforcing `org-agenda-sticky-auto-refresh'.

Intended as :filter-return advice on `org-agenda'."
  (when (and org-agenda-sticky-auto-refresh
             (equal result "Sticky Agenda buffer, use `r' to refresh"))
    (org-agenda-redo)))

(advice-add 'org-agenda :filter-return #'org-agenda-sticky-auto-refresh-advice)

(defun org-agenda-sticky-quit-advice (orig-fn)
  "Advice so `org-agenda-quit' buries sticky agendas properly.

When `org-agenda-sticky' is true and `org-agenda-window-setup' is
the symbol `reorganize-frame', `org-agenda-quit' has a bug where
the agenda does not get buried properly.  This advice restores
the intended behavior.

Intended as :around advice for `org-agenda-quit'."
  ;; The bug seems to stem from the fact that `bury-buffer' only
  ;; removes the current buffer from its window if its argument is
  ;; nil, not if its argument is the current buffer.  So we
  ;; temporarily advice `bury-buffer' so that it only gets an argument
  ;; when that argument is not current.
  (vizier-with-advice-if org-agenda-sticky
      ((bury-buffer :filter-args
                    (lambda (args)
                      (unless (equal args (list (current-buffer)))
                        (list (current-buffer))))))
    (funcall orig-fn)))

(advice-add 'org-agenda-quit :around #'org-agenda-sticky-quit-advice)


;;;; Unloading
;;============
(defun org-agenda-sticky-unload-function ()
  "Undo changes made to Emacs by the module `org-agenda-sticky'."
  (advice-remove 'org-agenda #'org-agenda-sticky-auto-refresh-advice)
  (advice-remove 'org-agenda-quit #'org-agenda-sticky-quit-advice))

(provide 'org-agenda-sticky)
;;; org-agenda-sticky.el ends here
