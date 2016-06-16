;;; aph-page.el --- Extensions for `smartparens'     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience editing

;; Dependencies: `smartparens'

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

;; Extensions for `smartparens'.

;;; Code:

(require 'smartparens)

;;;###autoload
(defun aph/sp-kill-sentence (&optional arg)
  "As `kill-sentence', but don't kill past end of current context.

In a string or comment, kill either to the end of sentence (as
`kill-sentence') or to the end of the string or comment,
whichever is nearer.  Do not kill a closing string or comment
delimiter.  Treat ARG in the same way as `kill-sentence'.

Outside of strings and comments, this should generally behave as
`kill-sentence', but no guarantees are made."
  (interactive "p")
  (let* ((arg              (or arg 1))
         (context          (sp--get-context))
         (compare          (if (< arg 0) #'> #'<))
         (end-of-sentence  (save-excursion (forward-sentence arg))))
    (kill-region (point)
                 (progn
                   (while (and (funcall compare (point) end-of-sentence)
                               (eq (sp--get-context) context))
                     (forward-char (sp--signum arg)))
                   (unless (eq (sp--get-context) context)
                     (backward-char (sp--signum arg)))
                   (point)))))

(provide 'aph-smartparens)
;;; aph-smartparens.el ends here
