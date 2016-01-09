;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HELM EXTENSIONS
;;;;============================================================================

;; Extensions to the `helm' package.
(require 'helm)


;;; Subroutines
;;;============
(defun aph/helm-append-keyword (keyword value)
  "Add KEYWORD and VALUE as args to next `helm' invocation."
  (require 'aph-advice)
  (aph/advice-once #'helm :filter-args
                   (lambda (args) 
                     (append args (list keyword value)))))

(defun aph/helm-suspend-update-initially ()
  "Suspend updates; remove self from `helm-after-initialize-hook'.

Add to `helm-after-initialize-hook' to cause the next `helm'
command invoked to start with updates suspended.

If that `helm' command specified that updates should start
suspended, this will instead reenable updates."
  (require 'aph-silence)
  (unwind-protect
      (let ((aph/silence-list    '("^Helm update suspended!$"))
            (aph/silence-enabled t))
        (helm-toggle-suspend-update))
    (remove-hook 'helm-after-initialize-hook
                 #'aph/helm-suspend-update-initially)))


;;; In-Session Commands
;;;====================
(defun aph/helm-resume-update-or-exit-minibuffer ()
  "Resume updates if suspended, else `helm-maybe-exit-minibuffer'."
  (interactive)
  (require 'aph-silence)
  (if helm-suspend-update-flag
      (let ((aph/silence-list    '("^Helm update reenabled!$"))
            (aph/silence-enabled t))
        (helm-toggle-suspend-update))
    (helm-maybe-exit-minibuffer)))


;;; Session-Starting Commands
;;;==========================
(defun aph/helm-browse-project (arg)
  "As `helm-browse-project', but truncate lines."
  (interactive "P")
  (aph/helm-append-keyword :truncate-lines t)
  (helm-browse-project arg))


(provide 'aph-helm)
