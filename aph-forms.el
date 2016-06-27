;;; aph-forms.el --- Extensions for `forms-mode'     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data

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

;; Custom functions and commands for use in `forms-mode'.

;;; Code:

;;;###autoload
(defun aph/forms-create-from-template (template dir name)
  "Make a new `forms-mode' database based on TEMPLATE.

Here, TEMPLATE is the path to an existing `forms-mode' control
file.  A new control file named NAME.ctrl is created in DIR, as
well as an empty database file NAME.db.  The resulting database
uses `load-file' to inherit all behavior except the value of
`forms-file' from TEMPLATE.

After the file is created, open it in `forms-mode'."
  (interactive "fCreate database from template: 
DCreate database in directory: 
sName for new database: ") 
  (let ((control-file  (expand-file-name (format "%s.ctrl" name) dir)))
    (with-temp-buffer
      (insert (concat ";; -*- mode: emacs-lisp -*-\n"
                      "\n"
                      (format "(load-file %S)\n" template)
                      (format "(setq forms-file \"%s.db\")\n" name)
                      "\n"
                      (format ";;; %s.ctrl ends here" name)))
      (write-region nil nil control-file nil nil nil :new))
    (forms-find-file control-file)))

(provide 'aph-forms)
;;; aph-forms.el ends here
