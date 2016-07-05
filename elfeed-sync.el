;;; elfeed-sync.el --- Sync Elfeed across machines   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience comm rss

;; Dependencies: `elfeed'

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

;; When accessing the same `elfeed' directory from multiple machines
;; (e.g., using a Dropbox folder), it's relatively easy for different
;; `elfeed' instances to get out of sync with one another.  This
;; module provides additional functionality that should make it easier
;; to keep everything in sync.
;;
;; For the moment, only one command is provided, `elfeed-sync-save'.
;; This is just an interactive version of `elfeed-db-save'.
;;
;; Future plans include automatically saving the database before
;; and/or after updating.

;;; Code:

(require 'elfeed)

;;;###autoload
(defun elfeed-sync-save ()
  "Save `elfeed' database.

This is just an interactive version of `elfeed-db-save'."
  (interactive)
  (when (elfeed-db-save)
    (message "Elfeed database saved")))

(provide 'elfeed-sync)
;;; elfeed-sync.el ends here
