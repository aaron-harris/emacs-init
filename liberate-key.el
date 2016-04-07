;;; liberate-key.el --- Separate keys from their roots -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience keybinding

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

;; Because Emacs is designed to be useful in a terminal, certain keys
;; are automatically remapped to their ASCII equivalents (most notably
;; <tab>, <return>, and <escape>).  This means that when running Emacs
;; in a GUI, those ASCII equivalents (C-i, C-m, and C-[, respectively)
;; are not usable separate from their function key equivalents.
;;
;; The process to separate C-i from <tab> is straightforward: just
;; bind separate functions to C-i and to <tab>, and Emacs figures
;; everything out.  The same goes for C-m and <return>.
;;
;; The situation with the <escape> key is more complicated, because of
;; the association between the <escape> key and the meta prefix.  The
;; sole function provided by this module, `liberate-key-escape',
;; attempts to fix the problem by altering `input-decode-map' so that
;; C-[ remaps to the symbol `<C-[>'; this happens before
;; `function-key-map' translates <escape> to C-[, so the net effect is
;; that the <escape> key and the meta key function as normal, and the
;; C-[ key is remapped to `<C-[>'.
;;
;; Note that, since `input-decode-map' is terminal-local, if you are
;; using emacsclient, then calling `liberate-key-escape' once is not
;; enough; instead, add it to `after-make-frame-functions'.

;;; Code:
(defun liberate-key-escape (&optional frame)
  "Decouple the escape key from its ASCII roots.

By default, the escape key and `C-[' resolve to the same
key (character code 27), even in a GUI.  This function breaks
that association (using `input-decode-map') so that the `C-[' key
can be used for other things.

After this function has been executed, the escape key will
continue to resolve to character code 27, but `C-[' will resolve
to the bare symbol `C-['.  This means that the relationship
between the escape key and the meta key is unaffected, but to
bind a key to `C-[' you should use (kbd \"<C-[>\") rather
than (kbd \"C-[\").

To make the change effective when using emacsclient, add this
function to `after-make-frame-functions'."
  (with-selected-frame (or frame (selected-frame))
    (define-key input-decode-map (kbd "C-[") (kbd "<C-[>"))))

(defun liberate-key-unload-function ()
  "Undo changes made to Emacs by `liberate-key-escape'.

This function just removes the remapping in `input-decode-map'.
No attempt is made to restore any binding that may have been
overwritten by `liberate-key-escape'; instead, it is restored to
the default nil binding."
  (define-key input-decode-map (kbd "C-[") nil))


(provide 'liberate-key)
;;; liberate-key.el ends here
