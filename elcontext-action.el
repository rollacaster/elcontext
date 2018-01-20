;;; elcontext.el --- Create context specific actions

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

(defun elcontext-action-run (context)
  "Run a CONTEXT action."
  (elcontext--mark-as-run context (current-time))
  (eval (ht-get context :action)))

(defun elcontext--mark-as-run (context time)
    "Update a CONTEXT with the last TIME it was run."
    (ht-set! context :last-run time)
    context)

(defun elcontext-action-valid-context (context)
  "Check if the CONTEXT did already run today."
  (when (or (not (ht-get context :last-run))
            (not (elcontext-action--same-day (ht-get context :last-run)
                                       (current-time))))
    t))

(defun elcontext-action--same-day (time1 time2)
  "Check if TIME1 and TIME2 have the same day."
  (let* ((today (decode-time time1))
         (year (nth 5 today))
         (month (nth 4 today))
         (day (nth 3 today))
         (context-last-run (decode-time time2))
         (context-year (nth 5  context-last-run))
         (context-month (nth 4  context-last-run))
         (context-day (nth 3  context-last-run)))
    (when (and (eq year context-year)
               (eq month context-month)
               (eq day context-day))
        t)))

(provide 'elcontext-action)

;;; elcontext-action.el ends here
