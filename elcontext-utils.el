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

(defun elcontext-utils-read-number-range (from to prompt &optional default)
  "Read a number range between FROM and TO with a PROMPT.

The optional DEFAULT is used when the user enters emtpy input."
  (let ((number))
    (while (not number)
      (let ((userInput (read-from-minibuffer prompt default)))
        (if (and
             (<= from (string-to-number userInput))
             (>= to (string-to-number userInput)))
            (setq number userInput)
          (read-from-minibuffer (format "Please specify a number between %d-%d." from to)))))
    number))

(provide 'elcontext-utils)

;;; elcontext-utils.el ends here
