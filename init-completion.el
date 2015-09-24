;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; COMPLETION SETTINGS
;;;;============================================================================


;;; Smart Tab
;;;==========
(when (aph/require-softly 'smart-tab)
  (global-smart-tab-mode 1)
  (setq smart-tab-using-hippie-expand t))
