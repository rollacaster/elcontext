;;; elcontext.el --- Create context specific actions -*- lexical-binding: t -*-

;; Copyright (C) 2018 Thomas Sojka

;; Author: Thomas Sojka

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

;;

;;; Code:

(require 'ht)

(setq elcontext-directory--current "")
(defun elcontext-directory-valid-context (context)
  "Check if the CONTEXT did already run today."
  (when (or (equal (ht-get context :directory) "")
            (equal (expand-file-name "" (ht-get context :directory))
                   (expand-file-name "" default-directory)))
    t))

(defhydra elcontext-directory-hydra (:hint nil :foreign-keys warn)
  "
_s_: Set directory    | %`elcontext-directory--current
_e_: Edit location |

_c_: Create directory
_q_: Quit
"
  ("s" (setq elcontext-directory--current (read-directory-name "Directory: ")))
  ("e" (setq elcontext-directory--current (read-directory-name "Directory: " elcontext--context-current)))
  ("c" (progn
         (ht-set! elcontext--context-current :directory elcontext-directory--current)
         (setq elcontext-directory--current "")
         (elcontext-hydra-create-context/body)) :exit t)
  ("q" (elcontext-hydra-create-context/body) :exit t))

(defun elcontext-directory-create (context)
  "Choose a new directory or a edit a existing CONTEXT directory from user input."
  (setq elcontext-directory--current (ht-get context :directory))
  (elcontext-directory-hydra/body))

(provide 'elcontext-directory)

;;; elcontext-directory.el ends here
