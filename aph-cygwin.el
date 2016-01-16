;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; CYGWIN EXTENSIONS
;;;;============================================================================

;; Functions extending Cygwin compatability features..


;;; Path Conversion
;;;================
(defun aph/cygwin-convert-file-name-to-hybrid-windows
    (file &optional absolute-p)
  "As `cygwin-convert-file-name-to-windows', with forward slashes."
  (replace-regexp-in-string "\\\\" "/"
                            (cygwin-convert-file-name-to-windows
                             file absolute-p)))


(provide 'aph-cygwin)
