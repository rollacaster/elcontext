;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)
(require 'hydra)
(require 'elcontext-time)
(require 'uuidgen)

(defvar elc-contexts (ht))

(defvar elcontext-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'elc-new-context)
    (define-key map (kbd "e") 'elc-edit-context)
    (define-key map (kbd "d") 'elc-delete-context)
    map)
  "Keymap for `elcontext-mode'.")


(define-derived-mode elcontext-mode tabulated-list-mode "Contexts"
  "Special mode for contexts."
  (setq mode-name "elcontext")
  (use-local-map elcontext-mode-map)
  (setq tabulated-list-format [("Name" 15 t) ("Location" 30 t) ("Time" 30 t) ("Action" 20 t)])
  (setq tabulated-list-entries 'elc-get-contexts-for-table)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun list-contexts ()
  "Manage contexts in Emacs."
  (interactive)
  (get-buffer-create "**Contexts**")
  (switch-to-buffer "**Contexts**")
  (elcontext-mode))

(defun elc-get-contexts-for-table ()
  "Return all context in table format."
  (ht-map (lambda (key context)
            (list key
                  (vector (ht-get context :name)
                          (elc--gps-to-string (ht-get context :location))
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
                    (< (elc--distance current (ht-get context :location)) 0.100)
                    (elc-time-within-timespanp (current-time) (ht-get context :time)))
                   (progn
                     (eval (ht-get context :action)))))
             elc-contexts)))

(setq elc--context-id nil)
(setq elc--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location nil)))

(defhydra hydra-context (:hint nil :foreign-keys warn)
      "
_n_: Change name     | Name     %(ht-get elc--context-current :name)
_l_: Change location | Location %(elc-location-gps-to-string (ht-get elc--context-current :location))
_t_: Change time     | Time     %(elc-time-timespan-to-string (ht-get elc--context-current :time))
_a_: Change action   | Action   %(ht-get elc--context-current :action)

_c_: Create context
"
      ("n" (ht-set! elc--context-current :name (read-from-minibuffer "Name: ")))
      ("l" (ht-set! elc--context-current :location (elc-location-get-gps)))
      ("t" (elc-time-create-timespan (ht-get elc--context-current :time)) :exit t)
      ("a" (ht-set! elc--context-current :action (read-minibuffer "Action: ")))
      ("c" (progn
             (elc-add-context elc--context-id elc--context-current)
             (setq elc--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location nil)))
             (tabulated-list-print))
       :color blue))

(defun elc-new-context ()
  "Create a new context."
  (interactive)
  (setq elc--context-id (uuidgen-4))
  (hydra-context/body))

(defun elc-edit-context ()
  "Edit context at point."
  (interactive)
  (let ((context-id (tabulated-list-get-id)))
    (setq elc--context-id context-id)
    (setq elc--context-current (ht-get elc-contexts context-id))
    (hydra-context/body)))

(defun elc-delete-context ()
  "Delete context at point."
  (interactive)
  (ht-remove! elc-contexts (tabulated-list-get-id))
  (tabulated-list-print))

(provide 'elcontext)

;;; elcontext.el ends here
