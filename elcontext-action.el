;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)

(defun elc-action-run (context)
  "Run a CONTEXT action."
  (elc--mark-as-run context (current-time))
  (eval (ht-get context :action)))

(defun elc--mark-as-run (context time)
    "Update a CONTEXT with the last TIME it was run."
    (ht-set! context :last-run time)
    context)

(defun elc-action-valid-context (context)
  "Check if the CONTEXT did already run today."
  (when (or (not (ht-get context :last-run))
            (not (elc-action--same-day (ht-get context :last-run)
                                       (current-time))))
    t))

(defun elc-action--same-day (time1 time2)
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
