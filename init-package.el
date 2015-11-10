;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; PACKAGE MANAGEMENT
;;;;============================================================================

(require 'package)

(defun aph/offline-p ()
  "Return non-nil if this machine should not install packages.
The return value depends only on `aph/machine'."
  (eq aph/machine 'mpc))

(setq package-archives
      (and (not (aph/offline-p))
           '(("elpa"      . "http://elpa.gnu.org/packages/")
             ("marmalade" . "https://marmalade-repo.org/packages/")
             ("melpa"     . "https://melpa.org/packages/"))))
(package-initialize)

;; Bootstrap `use-package'
(unless (or (package-installed-p 'use-package) (aph/offline-p))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(provide 'init-package)
