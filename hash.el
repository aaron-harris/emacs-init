;;; hash.el --- Extra stuff for hash functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `validate' (optional)

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

;; This module contains code related to cryptographic hash functions,
;; e.g. `md5', `sha1', etc.

;;; Code:


;;;; User Options
;;===============
(defgroup hash nil
  "Extra stuff for cryptographic hash functions."
  :prefix "hash-"
  :link   '(emacs-commentary-link "hash")
  :group  'convenience)

(defcustom hash-default 'md5
  "Default hash function used by `hash' module."
  :type '(choice (const md5)
                 (const sha1)
                 (const sha224)
                 (const sha256)
                 (const sha384)
                 (const sha512))) 

;;;###autoload
(defun hash-insert (string &optional hash)
  "Insert the hash of STRING into current buffer.
Interactively, prompt for STRING.

If HASH is not supplied, it defaults to `hash-default'."
  (interactive "sString to hash: ")
  (when (require 'validate nil :noerror)
    (validate-variable 'hash-default))
  (let ((hash (or hash hash-default)))
    (insert (secure-hash hash string))))

(provide 'hash)
;;; hash.el ends here
