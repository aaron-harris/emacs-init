;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HELM EXTENSIONS
;;;;============================================================================

;; Extensions to the `helm' package.
(require 'helm)


;;; Subroutines
;;;============
(defun aph/helm-suspend-initial-update ()
  "Suspend updates; remove self from `helm-after-initialize-hook'.

Add to `helm-after-initialize-hook' to cause the next `helm'
command invoked to start with updates suspended."
  (require 'aph-silence)
  (let ((aph/silence-list    '("^Helm update suspended!$"))
        (aph/silence-enabled t))
    (helm-toggle-suspend-update))
  (remove-hook 'helm-after-initialize-hook #'aph/helm-suspend-initial-update))


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


(provide 'aph-helm)
