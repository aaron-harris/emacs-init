;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; WINDOWS-SPECIFIC EXTENSIONS
;;;;============================================================================

;;; This file contains code specific to the Windows OS.

(defun aph/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])) 

(provide 'aph-w32)
