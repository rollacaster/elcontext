;;; elcontext.el --- Create context specific actions -*- lexical-binding: t -*-

;; Copyright (C) 2018 Thomas Sojka

;; Author: Thomas Sojka
;; Version: 1.0.0
;; Package-Requires: ((ht "2.3") (hydra "0.14.0") (emacs "24.3") (f "0.20.0") (osx-location "20150613.217") (uuidgen "0.3"))
;; Keywords: calendar, convenience
;; URL: https://github.com/rollacaster/elcontext

;; This file is not part of GNU Emacs.

;;; Commentary:

;; elcontext

;; Currently only macOS is supported.
;; Use (elcontext) for on overview of all contexts. Within this overview
;; several hydras will guide through the API, press ? to open the help hydra. A
;; contexts consists of a name, location, timespan and action. When the curernt
;; time is within the timespan and your current position within 100 meters of
;; the location the action is triggered once per day.

;;; Code:

(require 'ht)
(require 'hydra)
(require 'f)
(require 'uuidgen)
(require 'elcontext-time)
(require 'elcontext-action)
(require 'elcontext-location)
(require 'elcontext-directory)
(when (string-equal system-type "darwin")
  (require 'osx-location))

(defvar elcontext-contexts (ht))

(defface elcontext-success
  '((((class color)) :inherit 'success))
  "Green color indicating a context which did run today."
  :group 'elcontext)

(defvar elcontext-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'elcontext-new-context)
    (define-key map (kbd "e") 'elcontext-edit-context)
    (define-key map (kbd "d") 'elcontext-delete-context)
    (define-key map (kbd "?") 'elcontext-hydra-help/body)
    map)
  "Keymap for `elcontext-mode'.")


(define-derived-mode elcontext-mode tabulated-list-mode "Contexts"
  "Special mode for contexts."
  (setq mode-name "elcontext")
  (use-local-map elcontext-mode-map)
  (setq tabulated-list-format
        (vconcat (vector '("Name" 15 t))
                 (when (string-equal system-type "darwin") (vector '("Location" 25 t)))
                 (vector '("Time" 25 t) '("Directory" 25 t) '("Action" 25 t))))
  (setq tabulated-list-entries 'elcontext--get-contexts-for-table)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun elcontext ()
  "Manage contexts in Emacs."
  (interactive)
  (get-buffer-create "**Contexts**")
  (switch-to-buffer "**Contexts**")
  (elcontext-mode))

(defun elcontext--get-contexts-for-table ()
  "Return all context in table format."
  (ht-map (lambda (key context)
            (list key
                  (vconcat
                   (vector (if (elcontext-action-valid-context context)
                               (ht-get context :name)
                             (propertize  (if (ht-get context :name) (ht-get context :name) "") 'face 'elcontext-success)))
                   (when (string-equal system-type "darwin")
                     (vector (elcontext-location-to-string context)))
                   (vector (elcontext-time-to-string context)
                           (ht-get context :directory)
                           (prin1-to-string (ht-get context :action))))))
          elcontext-contexts))

(defun elcontext-add-context (id context)
  "Store a context with ID and CONTEXT."
  (ht-set! elcontext-contexts id context))

(defun elcontext-check-contexts ()
  "Execute contexts if they are valid."
  (interactive)
  (ht-each (lambda (name context)
             (if (and
                  (elcontext-action-valid-context context)
                  (elcontext-location-valid-context context)
                  (elcontext-time-valid-context context)
                  (elcontext-directory-valid-context context))
                 (elcontext-action-run context)))
           elcontext-contexts))

(setq elcontext--timer nil)
(define-minor-mode elcontext-global-mode
  "Toogle elcontext-mode. Checks every minute for valid contexts"
  :lighter "elc"
  :group 'elcontext
  :global t
  :require 'elcontext
  (if (symbol-value 'elcontext-global-mode)
      (progn
        (setq elcontext--timer (run-at-time nil 5 'elcontext-check-contexts))
        (when (string-equal system-type "darwin")
          (osx-location-watch)))
    (progn
      (cancel-timer elcontext--timer)
      (setq elcontext--timer nil))))

(setq elcontext--context-id nil)
(setq elcontext--context-current
      (ht (:name nil) (:time (ht)) (:action nil) (:location (ht)) (:directory "")))

(defhydra elcontext-hydra-create-context (:hint nil :foreign-keys warn)
  (concat "_n_: Change name     | Name     %(ht-get elcontext--context-current :name)"
          (when (string-equal system-type "darwin") "
_l_: Change location  | Location %(elcontext-location-to-string elcontext--context-current)")
 "
_t_: Change time      | Time     %(elcontext-time-to-string elcontext--context-current)
_d_: Change directory | Directory   %(ht-get elcontext--context-current :directory)
_a_: Change action    | Action   %(ht-get elcontext--context-current :action)

_c_: Create context
_q_: Quit
")
      ("n" (ht-set! elcontext--context-current :name (read-from-minibuffer "Name: ")))
      ("l" (elcontext-location-create elcontext--context-current) :exit t)
      ("t" (elcontext-time-create elcontext--context-current) :exit t)
      ("d" (elcontext-directory-create elcontext--context-current) :exit t)
      ("a" (ht-set! elcontext--context-current :action (read-minibuffer "Action: ")))
      ("c" (progn
             (elcontext-add-context elcontext--context-id elcontext--context-current)
             (setq elcontext--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location (ht))))
             (tabulated-list-print)) :color blue)
      ("q" (progn
             (setq elcontext--context-id nil)
             (setq elcontext--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location (ht))))) :exit t))

(defhydra elcontext-hydra-help (:hint nil :exit t)
      "
**elcontext help**

_c_: Create context
_e_: Edit context
_d_: Delete context

_q_: Quit
"
      ("c" (elcontext-new-context))
      ("e" (elcontext-edit-context))
      ("d" (elcontext-delete-context))
      ("q" nil))

(defun elcontext-new-context ()
  "Create a new context."
  (interactive)
  (setq elcontext--context-id (uuidgen-4))
  (elcontext-hydra-create-context/body))

(defun elcontext-edit-context ()
  "Edit context at point."
  (interactive)
  (let ((context-id (tabulated-list-get-id)))
    (setq elcontext--context-id context-id)
    (setq elcontext--context-current (ht-get elcontext-contexts context-id))
    (condition-case nil
        (elcontext-hydra-create-context/body)
      (wrong-type-argument (user-error "No context found at point")))))

(defun elcontext-delete-context ()
  "Delete context at point."
  (interactive)
  (let ((context (ht-get elcontext-contexts (tabulated-list-get-id))))
    (condition-case nil
        (when (y-or-n-p (concat "Delete context " (ht-get context :name) "?"))
          (ht-remove! elcontext-contexts (tabulated-list-get-id))
          (tabulated-list-print))
      (wrong-type-argument (user-error "No context found at point")))))

(defun elcontext--save-contexts ()
  "Save contexts to disk."
  (f-write-text (prin1-to-string elcontext-contexts) 'utf-8
                (expand-file-name ".contexts" user-emacs-directory)))

(add-hook 'kill-emacs-hook 'elcontext--save-contexts)

(defun elcontext--load-contexts ()
  "Load contexts from disc."
  (when (f-exists? (expand-file-name ".contexts" user-emacs-directory))
    (let ((saved-contexts (read (f-read-text (expand-file-name ".contexts" user-emacs-directory)))))
      (if (ht? saved-contexts)
          (setq elcontext-contexts saved-contexts)
        (setq elcontext-contexts (ht))))))

(elcontext--load-contexts)

(provide 'elcontext)

;;; elcontext.el ends here
