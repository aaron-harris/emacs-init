;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; ALIGN EXTENSIONS
;;;;============================================================================

;; Extensions for `align' package.
(require 'align)


;;; Basic Extensions
(defun aph/align (beg end &optional separate rules exclude-rules)
  "As `align', but always use prefix argument."
  (interactive "r")
  (let ((current-prefix-arg '(4)))
    (align beg end separate rules exclude-rules)))


(provide 'aph-align)
