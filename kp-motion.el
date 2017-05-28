;;; kp-motion.el --- Use keypad enter to move        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

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

;; This module defines the (very simple) minor mode `kp-motion-mode'.
;; When this mode is enabled, the enter key on the keypad will be used
;; for downward motion (`next-line') instead of its normal function.
;; That's it.

;;; Code:

(defgroup kp-motion nil
  "Use enter on keypad for downward motion."
  :prefix "kp-motion-"
  :link '(emacs-commentary-link "kp-motion")
  :group 'convenience)

;;;###autoload
(define-minor-mode kp-motion-mode
  "Simple minor mode enabling motion with enter key on keypad.

When this mode is enabled, the enter key on the
keypad (`<kp-enter>') will be used for downward
motion (`next-line') instead of its normal function."
  :lighter " KP"
  :keymap `((,(kbd "<kp-enter>") . next-line)))

(provide 'kp-motion)
;;; kp-motion.el ends here
