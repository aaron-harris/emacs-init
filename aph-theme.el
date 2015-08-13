;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; THEME FUNCTIONS
;;;;============================================================================

(defvar aph/theme-day 'zenburn
  "The theme to use during the day.")

(defvar aph/theme-night 'hc-zenburn
  "The theme to use during the night.")

(defun aph/theme-night-toggle ()
  "Toggle between themes `aph/theme-day' and `aph/theme-night'.

If neither of these themes is currently active, load `aph/theme-night'." 
  (interactive)
  (if (custom-theme-enabled-p aph/theme-night)
      (progn
        (disable-theme aph/theme-night)
        (load-theme aph/theme-day :noconfirm))
    (disable-theme aph/theme-day)
    (load-theme aph/theme-night :noconfirm)))

(provide 'aph-theme)
