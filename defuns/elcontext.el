;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)
(require 'prodigy)
(require 'hydra)
(require 'elcontext-time)

(defvar elc-contexts (ht))

(define-derived-mode elcontext-mode tabulated-list-mode "Contexts"
  "Special mode for contexts."
  (setq tabulated-list-format [("Name" 15 t) ("Location" 30 t) ("Time" 15 t) ("Action" 20 t)])
  (setq tabulated-list-entries 'elc-get-contexts-for-table)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun list-contexts ()
  "Manage contexts in Emacs."
  (interactive)
  (get-buffer-create "elcontext")
  (switch-to-buffer "elcontext")
  (elcontext-mode))

(defun elc-get-contexts-for-table ()
  "Return all context in table format."
  (ht-map (lambda (key context)
            (list (ht-get context :name)
                  (vector key
                          (elc--gps-to-string (ht-get* context :location :gps))
                          (elc-time-timespan-to-string (ht-get context :time))
                          (format "%s" (ht-get context :action)))))
          elc-contexts))

(defun elc-add-context (contextName context)
  "Store a context with CONTEXTNAME and CONTEXT."
  (ht-set! elc-contexts contextName context))

(defun elc-check-contexts ()
  "Execute contexts if they are valid."
  (interactive)
  (let ((current (elc--get-gps)))
    (ht-each (lambda (name context)
               (if (and
                    (< (elc--distance current (ht-get* context :location :gps)) 0.100)
                    (elc-time-within-timespanp (current-time) (ht-get context :time)))
                   (progn
                     (funcall (ht-get context :action))
                     (message (concat "Run " name)))))
             elc-contexts)))

(setq elc--context-current (ht (:name nil) (:time nil) (:action nil) (:location nil)))

(defhydra hydra-context (:hint nil
                                   :foreign-keys run)
      "
_n_: Change name     | Name     %(ht-get elc--context-current :name)
_l_: Change location | Location %(elc-location-gps-to-string (ht-get elc--context-current :location))
_t_: Change time     | Time     %(elc-time-timespan-to-string (ht-get elc--context-current :time))
_a_: Change action   | Action   %(ht-get elc--context-current :action)

_c_: Create context
"
      ("n" (ht-set! elc--context-current :name (read-from-minibuffer "Name: ")))
      ("l" (ht-set! elc--context-current :location (elc-location-get-gps)))
      ("t" (elc-create-timespan) :exit t)
      ("a" (ht-set! elc--context-current :action (read-minibuffer "Action: ")))
      ("c" (progn
             (elc-add-context elc--context-name elc--context-current)
             (setq elc--context-current (ht (:name nil) (:time nil) (:action nil) (:location nil))))
       :color blue))

(defun elc-new-context ()
  "Create a new context."
  (interactive)
  (hydra-context/body))


(provide 'elcontext)

;;; elcontext.el ends here
