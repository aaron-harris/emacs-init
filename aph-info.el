;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; INFO MODE COMMANDS
;;;;============================================================================

;; This file contains commands for use in `Info-mode'.

;;;###autoload
(defun aph/info-mode-or-clone-buffer (prefix)
  "Enter info mode or clone info buffer.

In an info buffer when no prefix argument has been supplied,
clone the buffer (as `clone-buffer').  Otherwise, enter info
mode (as `info')."
  (interactive "P")
  (if (and (eq major-mode 'Info-mode) (null prefix))
      (clone-buffer (not :rename) :popto)
    (setq prefix-arg prefix)
    (call-interactively #'info)))

;;;###autoload
(defun aph/Info-final-menu-item ()
  "Go to the node of the last menu item.

This command duplicates the functionality of the 0 key in the
standalone info application."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting nil)))
