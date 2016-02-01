;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HELM EXTENSIONS FOR FORMS MODE
;;;;============================================================================

;; Helm extensions for `forms-mode'.
(require 'aph-forms)
(require 'helm)


;;; Helm Sources
(defun aph/helm-forms-list-records ()
  "As `aph/forms-list-records', but called from helm."
  (with-helm-current-buffer (aph/forms-list-records)))

(defvar aph/helm-forms-all-records-source 
  '((name       . "Records")
    (candidates . aph/helm-forms-list-records)
    (action     . forms-jump-record)))

(defun aph/helm-forms-jump-record ()
  "Use `helm' to jump to a record in current `forms-mode' db."
  (interactive)
  (helm :sources '(aph/helm-forms-all-records-source)))

(provide 'aph-helm-forms)
