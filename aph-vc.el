;;; aph-vc.el --- Extensions for `vc'                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: vc tools

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

;; Functions and commands extending those found in the `vc' module
;; built into Emacs.

;;; Code:

(require 'vc)

;;;###autoload
(defun aph/vc-delete-file (file)
  "As `vc-delete-file', but delete unregistered files.

If the command `vc-delete-file' is invoked on a file which is not
under version control, an error is signaled.  This command
instead deletes the file, requesting confirmation from the user.
In all other circumstances, it behaves as `vc-delete-file'."
  (interactive (list (read-file-name "VC delete file: " nil
                                     (when (vc-backend buffer-file-name)
                                       buffer-file-name) t)))
  (cond
   ((vc-backend (expand-file-name file))               (vc-delete-file file))
   ((y-or-n-p (format "Really delete file %s?" file))  (delete-file file))))

;;;###autoload
(defun aph/vc-dir-delete-file ()
  "As `vc-dir-delete-file', but delete unregistered files.

See `aph/vc-delete-file' for more details; this is just a simple
wrapper around that function for use in the `vc-dir' buffer."
  (interactive)
  (mapc 'aph/vc-delete-file (or (vc-dir-marked-files)
                                (list (vc-dir-current-file)))))

(provide 'aph-vc)
;;; aph-vc.el ends here
