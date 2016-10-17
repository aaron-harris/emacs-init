;;; which-func-header.el --- Show `which-func` info in header -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, mode-line, imenu

;; Dependencies: `map'

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

;; This module defines the minor mode `which-func-header-mode'.  This
;; mode is an extension to `which-function-mode' that places the
;; information in the header line, rather than the mode line.
;;
;; Unlike `which-function-mode', `which-func-header-mode' is
;; buffer-local.  A globalized mode is planned, but has not yet been
;; implemented.  Therefore, to use `which-func-header-mode', you
;; should put `which-func-header-mode' in the mode hooks for whatever
;; major modes strike you as appropriate.

;;; Code:

(require 'which-func)
(require 'map)

(defcustom which-func-header-format
  '(which-function-mode (which-func-header-mode ("" which-func-format)))
  "Header line format construct for `which-func-header-mode'.

When `which-func-header-mode' is on, this will be used as
`header-line-format' (overriding any previous value)."
  :type 'sexp)

;;;###autoload
(define-minor-mode which-func-header-mode
  "Minor mode moving `which-function-mode' info to header.

This minor mode causes information normally shown by
`which-function-mode' in the mode line to instead be shown in the
header line.  If `which-function-mode' is disabled, nothing is
shown in either the header line or the mode line.

Unlike `which-function-mode', this mode is buffer-local."
  :group       'which-func
  :init-value  nil
  :require     'which-func-header
  (if (and which-function-mode which-func-header-mode)
      (let ((construct (assq 'which-func-mode mode-line-misc-info)))
        (when construct
          (map-delete mode-line-misc-info 'which-func-mode)
          (map-delete mode-line-misc-info 'which-func-header-mode)
          (push `(which-func-header-mode "" ,construct) mode-line-misc-info))
        (setq header-line-format which-func-header-format))
    (kill-local-variable 'header-line-format))
  (add-hook 'which-function-mode-hook
            #'which-func-header-mode:refresh nil :local))

(defun which-func-header-mode:on ()
  "Turn on `which-func-header-mode'."
  (which-func-header-mode 1))

(defun which-func-header-mode:off ()
  "Turn off `which-func-header-mode'."
  (which-func-header-mode -1))

(defun which-func-header-mode:refresh ()
  "Refresh header line display for `which-func-header-mode'.

It is possible to toggle `which-function-mode' independently of
`which-func-header-mode'.  Doing so can cause the contents of the
header line to become out of sync with the state of
`which-func-header-mode'.  To prevent this,
`which-func-header-mode' puts this function (which just
re-asserts the current value of `which-func-header-mode' in
`which-functino-mode-hook'.)"
  (which-func-header-mode (if which-func-header-mode 1 -1)))

(defun which-func-header-unload-function ()
  "Undo changes made to Emacs for `which-func-header-mode'."
  ;; Turn off `which-func-header-mode' in all buffers.
  ;; This should restore changes to `header-line-format'.
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when which-func-header-mode (which-func-header-mode -1))))
        (buffer-list))
  ;; Restore changes to `mode-line-misc-info'.
  (let ((construct (nth 2 (assq 'which-func-header-mode mode-line-misc-info))))
    (when construct
      (map-delete mode-line-misc-info 'which-func-header-mode)
      (push construct mode-line-misc-info)))
  ;; Let normal unloading proceed
  nil)

(provide 'which-func-header)
;;; which-func-header.el ends here
