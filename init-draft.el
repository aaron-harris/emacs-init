;;; init-draft.el --- Draft code for Emacs config    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris
;; Author: Aaron Harris <meerwolf@gmail.com>

;;; Commentary:

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.

;;; Code:


;;;; EIMP
;;=======
(defun aph/eimp-fit-image-to-window (&optional arg)
    "As `eimp-fit-image-to-window', but do not mark buffer as modified.
Also, ARG is optional, defaulting to nil."
    (let ((buffer     (current-buffer))
          (advice-id  (cl-gensym "aph/eimp-fit-image-to-window:")))
      ;; This is clearly a hack, and I should probably find a better
      ;; way to do this.
      (advice-add #'eimp-mogrify-process-sentinel :after
                  (lambda (proc msg)
                    (cond
                     ((not (buffer-live-p buffer))
                      (advice-remove #'eimp-mogrify-process-sentinel advice-id))
                     ((eq (process-buffer proc) buffer)
                      (with-current-buffer buffer
                        (set-buffer-modified-p nil)))))
                  `((name . ,advice-id)))
      ;; This wrapper fixes the situation where the current buffer is
      ;; not in the selected window, e.g., if we just opened the file
      ;; using `find-file-other-window'.
      (if (equal (selected-window) (get-buffer-window buffer))
          (eimp-fit-image-to-window arg) 
        (with-selected-window (next-window) ; Super-kludgy!
          (with-current-buffer buffer
            (eimp-fit-image-to-window arg))))))

(provide 'init-draft)
;; init-draft.el ends here
