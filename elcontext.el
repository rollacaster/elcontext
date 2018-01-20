;;; elcontext --- Define context specific services in emacs

;; Author: Thomas Sojka
;; Version: 1.0.0
;; Package-Requires ((ht) (hydra) (f) (deferred) (uuidgen) (s) (dash) (calendar))
;; Keywords: context, time, timespan, location, gps, action
;; URL: https://github.com/rollacaster/elcontext

;;; Commentary:

;; elcontext

;; Currently macOS and CoreLocationCLI must be installed.
;; Use (list-contexts) for on overview of all contexts. Within this overview
;; several hydras will guide through the API, press ? to open the help hydra. A
;; contexts consists of a name, location, timespan and action. When the curernt
;; time is within the timespan and your current position within 100 meters of
;; the location the action is triggered once per day.

(require 'ht)
(require 'hydra)
(require 'f)
(require 'deferred)
(require 'uuidgen)
(require 'elcontext-time)
(require 'elcontext-action)
(require 'elcontext-location)

(defvar elc-contexts (ht))

(defface elc-gray-face
  '((((class color)) :foreground "#787878"))
  "Gray color indicating a context which did not run yet."
  :group 'elcontext)

(defface elc-green-face
  '((((class color)) :foreground "#61b361"))
  "Green color indicating a context which did run today."
  :group 'elcontext)

(defvar elc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'elc-new-context)
    (define-key map (kbd "e") 'elc-edit-context)
    (define-key map (kbd "d") 'elc-delete-context)
    (define-key map (kbd "?") 'elc-hydra-help/body)
    map)
  "Keymap for `elcontext-mode'.")


(define-derived-mode elcontext-mode tabulated-list-mode "Contexts"
  "Special mode for contexts."
  (setq mode-name "elcontext")
  (use-local-map elc-mode-map)
  (setq tabulated-list-format [("Name" 15 t) ("Location" 30 t) ("Time" 30 t) ("Action" 20 t)])
  (setq tabulated-list-entries 'elc--get-contexts-for-table)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun list-contexts ()
  "Manage contexts in Emacs."
  (interactive)
  (get-buffer-create "**Contexts**")
  (switch-to-buffer "**Contexts**")
  (elcontext-mode))

(defun elc--get-contexts-for-table ()
  "Return all context in table format."
  (ht-map (lambda (key context)
            (list key
                  (vector (propertize  (if (ht-get context :name) (ht-get context :name) "") 'face (if (elc-action-valid-context context) 'elc-gray-face 'elc-green-face))
                          (elc-location-to-string context)
                          (elc-time-to-string context)
                          (prin1-to-string (ht-get context :action)))))
          elc-contexts))

(defun elc-add-context (id context)
  "Store a context with ID and CONTEXT."
  (ht-set! elc-contexts id context))

(defun elc-check-contexts ()
  "Execute contexts if they are valid."
  (interactive)
  (ht-each (lambda (name context)
             (lexical-let ((context context))
               (deferred:$
                 (elc-location-valid-context context)
                 (deferred:nextc it (lambda (valid-location)
                                      (if (and
                                           (elc-action-valid-context context)
                                           valid-location
                                           (elc-time-valid-context context))
                                          (elc-action-run context)))))))
           elc-contexts))

(setq elcontext--timer nil)
(define-minor-mode elcontext-global-mode
  "Toogle elcontext-mode. Checks every minute for valid contexts"
  :lighter "elc"
  :group 'elcontext
  :global t
  (if (symbol-value 'elcontext-global-mode)
      (setq elcontext--timer (run-at-time nil 5 'elc-check-contexts))
    (progn
      (cancel-timer elcontext--timer)
      (setq elcontext--timer nil))))

(setq elc--context-id nil)
(setq elc--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location (ht))))

(defhydra elc-hydra-create-context (:hint nil :foreign-keys warn)
      "
_n_: Change name     | Name     %(ht-get elc--context-current :name)
_l_: Change location | Location %(elc-location-to-string elc--context-current)
_t_: Change time     | Time     %(elc-time-to-string elc--context-current)
_a_: Change action   | Action   %(ht-get elc--context-current :action)

_c_: Create context
_q_: Quit
"
      ("n" (ht-set! elc--context-current :name (read-from-minibuffer "Name: ")))
      ("l" (elc-location-create elc--context-current) :exit t)
      ("t" (elc-time-create elc--context-current) :exit t)
      ("a" (ht-set! elc--context-current :action (read-minibuffer "Action: ")))
      ("c" (progn
             (elc-add-context elc--context-id elc--context-current)
             (setq elc--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location (ht))))
             (tabulated-list-print)) :color blue)
      ("q" (progn
             (setq elc--context-id nil)
             (setq elc--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location (ht))))) :exit t))

(defhydra elc-hydra-help (:hint nil :exit t)
      "
**elcontext help**

_c_: Create context
_e_: Edit context
_d_: Delete context

_q_: Quit
"
      ("c" (elc-new-context))
      ("e" (elc-edit-context))
      ("d" (elc-delete-context))
      ("q" nil))

(defun elc-new-context ()
  "Create a new context."
  (interactive)
  (setq elc--context-id (uuidgen-4))
  (elc-hydra-create-context/body))

(defun elc-edit-context ()
  "Edit context at point."
  (interactive)
  (let ((context-id (tabulated-list-get-id)))
    (setq elc--context-id context-id)
    (setq elc--context-current (ht-get elc-contexts context-id))
    (condition-case nil
        (elc-hydra-create-context/body)
      (wrong-type-argument (user-error "No context found at point")))))

(defun elc-delete-context ()
  "Delete context at point."
  (interactive)
  (let ((context (ht-get elc-contexts (tabulated-list-get-id))))
    (condition-case nil
        (when (y-or-n-p (concat "Delete context " (ht-get context :name) "?"))
          (ht-remove! elc-contexts (tabulated-list-get-id))
          (tabulated-list-print))
      (wrong-type-argument (user-error "No context found at point")))))

(defun elc--save-contexts ()
  "Save contexts to disk."
  (f-write-text (prin1-to-string elc-contexts) 'utf-8
                (expand-file-name ".contexts" user-emacs-directory)))

(add-hook 'kill-emacs-hook 'elc--save-contexts)

(defun elc--load-contexts ()
  "Load contexts from disc."
  (when (f-exists? (expand-file-name ".contexts" user-emacs-directory))
    (let ((saved-contexts (read (f-read-text (expand-file-name ".contexts" user-emacs-directory)))))
      (if (ht? saved-contexts)
          (setq elc-contexts saved-contexts)
        (setq elc-contexts (ht))))))

(elc--load-contexts)

(provide 'elcontext)

;;; elcontext.el ends here
