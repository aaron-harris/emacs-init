;;; forms-narrow.el --- Narrowing for `forms-mode'   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data, forms

;; Dependencies `forms', `seq'

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

;; This module implements a form of narrowing for `forms-mode'
;; databases.  When narrowing is in effect, some records are "hidden",
;; so that navigation commands will skip over them.  Note that this
;; functionality is distinct from the built-in narrowing functionality
;; (e.g., `narrow-to-region'), which cannot affect more than a single
;; record in `forms-mode'.
;;
;; To use this module, just open a `forms-mode' database and invoke a
;; narrowing command.  At present, there is only one narrowing
;; command, but more are planned:
;;
;; `forms-narrow-regexp':
;;
;;     Show only records matching the specified regexp (in any field).
;;
;; To remove the narrowing effect, use the command
;; `forms-narrow-widen'.  There is also the command
;; `forms-narrow-again', which applies whatever narrowing was last
;; used this session.
;;
;; None of these commands have default keybindings, but they are
;; collected in the map `forms-narrow-map' (not
;; `forms-narrow-mode-map'; see below) for use as a prefix map.
;; Moreoever, if you wish to use this map instead of the default
;; narrowing map at `C-x n' in `forms-mode' buffers, just add the
;; function `forms-narrow-shadow' to `forms-mode-hook'.
;;
;; Custom narrowing commands are also supported.  Just pass a function
;; of no arguments to the function `forms-narrow'.  A record will only
;; be shown if your function returns non-nil when that record is
;; current.
;;
;; Replacement of the basic `forms-mode' navigation commands (e.g.,
;; `forms-next-record') with narrowing-aware versions is implemented
;; via the minor mode `forms-narrow-mode', but in general you should
;; not need to enable this mode directly, as the narrowing and
;; widening commands will turn it on and off as necessary.
;;
;; Note that the set of navigation commands that support narrowing is
;; still incomplete.  Commands that are currently supported are as
;; follows (with their narrowing-aware counterparts):
;;
;;   `forms-next-record' => `forms-narrow-next-record'
;;   `forms-prev-record' => `forms-narrow-prev-record'
;;
;; Using navigation commands other than those listed here will ignore
;; the narrowing effect.

;;; Code:

(require 'forms)

(require 'seq)


;;;; State Variables
;;==================
(defvar-local forms-narrow--predicate nil
  "Predicate determining which records to show.

This predicate is called with no arguments whenever a new record
is displayed.  If it returns nil, that record is skipped.

If this variable is nil, all records are visible.")


;;;; Navigation Commands
;;======================
(defun forms-narrow-next-record (arg)
  "As `forms-next-record', but obey `forms-narrow--predicate'.

If all records after the current one satisfy
`forms-narrow--predicate', then signal an error and stay on this
record."
  (interactive "p")
  (if (null forms-narrow--predicate)
      (forms-next-record arg)
    (let ((stepper    (if (< arg 0) #'forms-prev-record #'forms-next-record))
          (saved-rec  forms--current-record))
      (condition-case err
          (dotimes (i (abs arg))
            (while (progn (funcall stepper 1)
                          (not (funcall forms-narrow--predicate))))
            (setq saved-rec forms--current-record))
        (error (forms-jump-record saved-rec)
               (signal (car err) (cdr err)))))))

(defun forms-narrow-prev-record (arg)
  "As `forms-prev-record', but obey `forms-narrow--predicate'."
  (interactive "p")
  (forms-narrow-next-record (- arg)))


;;;; Minor Mode and Keymap Setup
;;==============================
(defvar forms-narrow-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair
             '((forms-next-record . forms-narrow-next-record)
               (forms-prev-record . forms-narrow-prev-record)))
      (define-key keymap `[remap ,(car pair)] (cdr pair))) 
    keymap)
  "Keymap for `forms-narrow-mode'.")

(define-minor-mode forms-narrow-mode
  "Minor mode for narrowed `forms-mode' databases."
  :group forms
  :lighter " Narrow"
  :keymap  'forms-narrow-mode-map
  :require 'forms-narrow)

(defvar forms-narrow-map
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair
             '(("w" . forms-narrow-widen)
               ("r" . forms-narrow-regexp)
               ("a" . forms-narrow-again)))
      (define-key keymap (kbd (car pair)) (cdr pair))) 
    keymap)
  "Prefix map for narrowing commands in `forms-mode'.

By default, this prefix map is not bound to any key.  If you wish
to replace the default narrowing map (e.g., at `C-x n'), see the
function `forms-narrow-shadow'.")

;;;###autoload
(defun forms-narrow-shadow ()
  "Bind `forms-narrow-map' to `C-x n' in this buffer.

This binding will shadow entirely other, lower-precedence
keymaps (rather than merging their bindings).

If you want this to be the default in `forms-mode', add this
function to `forms-mode-hook'."
  (define-key forms-narrow-map (kbd "C-h") nil)
  (define-key forms-narrow-map [t] #'undefined)
  (local-set-key (kbd "C-x n") forms-narrow-map))


;;;; Entry and Exit Points
;;========================
;;;###autoload
(defun forms-narrow (pred)
  "Narrow the database to show only records satisfying PRED.
For use in `forms-mode'."
  (forms-narrow-mode 1)
  (setq forms-narrow--predicate pred))

(defun forms-narrow-widen (&optional verbose)
  "Remove narrowing for current database."
  (interactive "p")
  (when verbose (message "Widening database"))
  (forms-narrow-mode -1))

;;;###autoload
(defun forms-narrow-again (&optional verbose)
  "Narrow current database using last narrowing critera."
  (interactive "p")
  (when verbose (message "Narrowing database using last-used criteria"))
  (if (null forms-narrow--predicate)
      (error "No previous narrowing to re-use")
    (forms-narrow-mode 1)))


;;;; Narrowing Commands
;;=====================
;;;###autoload
(defun forms-narrow-regexp (regexp)
  "Narrow to records matching REGEXP in any field."
  (interactive "sNarrow with regexp: ")
  (forms-narrow
   (lambda ()
     (seq-some (apply-partially #'string-match-p regexp)
               (cdr forms-fields)))))

(provide 'forms-narrow)
;;; forms-narrow.el ends here
