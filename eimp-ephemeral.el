;;; eimp-ephemeral.el --- Ephemeral EIMP transforms  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; By default, applying any EIMP transformation to an image marks the
;; image buffer as modified.  This can be inconvenient if you do not
;; want to save the effect of the transformation.  This module
;; provides commands that invoke EIMP transformations without marking
;; the image buffer as modified.

;;; Code:

(require 'eimp)

(provide 'eimp-ephemeral)
;;; eimp-ephemeral.el ends here
