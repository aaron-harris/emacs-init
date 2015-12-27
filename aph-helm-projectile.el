;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HELM EXTENSIONS
;;;;============================================================================

;; Extensions to the `helm-projectile' module.
(require 'helm-projectile)
(require 'aph-helm)                     ; For `aph/helm-suspend-initial-update'


;;; Helm Commands
;;;==============
(defun aph/helm-projectile-grep (&optional dir)
  "As `helm-projectile-grep', but suspend updates initially."
  (interactive) 
  (add-hook 'helm-after-initialize-hook #'aph/helm-suspend-initial-update)
  (helm-projectile-grep dir))


(provide 'aph-helm-projectile)
