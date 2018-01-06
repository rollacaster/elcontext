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

(defun elc--get-gps ()
  "Return the current gps."
  (with-temp-buffer ()
    (call-process "whereami" nil t)
    (let ((lat (buffer-substring 11 20))
          (lon (buffer-substring 32 41)))
      (ht (:lat (string-to-number lat)) (:lon (string-to-number lon))))))


(defun elc--gps-to-string (gps)
  "Convert GPS coordinate in a readable format."
  (let ((lat (ht-get gps :lat))
        (lon (ht-get gps :lon)))
    (concat "Lat: " (number-to-string lat) " Lon: " (number-to-string lon))))

(defun elc--distance (from to)
  "Comutes the distance between FROM and TO in km."
  (let ((earth-radius 6371))
    (* 2 earth-radius (asin (sqrt (elc--haversine from to))))))

(defun elc--haversine (from to)
  "Computes the haversine for two coordinates FROM and TO."
  (let ((dLat (elc--degree-to-radians (abs (- (ht-get to :lat) (ht-get from :lat)))))
        (dLon (elc--degree-to-radians (abs (- (ht-get to :lon) (ht-get from :lon)))))
        (lat1 (elc--degree-to-radians (ht-get from :lat)))
        (lat2 (elc--degree-to-radians (ht-get to :lat))))
    (+ (expt (sin (/ dLat 2)) 2) (* (cos lat1) (cos lat2) (expt (sin (/ dLon 2)) 2)))))

(defun elc--get-lat (context)
  "Get the latitute for a CONTEXT."
  (ht-get* elc-contexts context :location :gps :lat))

(defun elc--get-lon (context)
  "Get the longitude for a CONTEXT."
  (ht-get* elc-contexts context :location :gps :lon))

(defun elc-add-context (contextName context)
  "Store a context with CONTEXTNAME and CONTEXT."
  (ht-set! elc-contexts contextName context))

(defun elc--degree-to-radians (degrees)
  "Convert DEGREES to radians."
  (/ (* degrees pi) 180))

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

(setq elc--context-name "")
(setq elc--context-location (ht))
(setq elc--context-time (ht))
(setq elc--context-action "")

(defun elc-new-context ()
  "Create a new context."
  (interactive)
  (progn
    (setq elc--context-name (read-from-minibuffer "Name: "))
    (let ((location "") (time "") (action ""))
      (defhydra hydra-context (:hint nil
                                     :foreign-keys run)
        "
_n_: Change name     | Name     %`elc--context-name
_l_: Change location | Location %`location
_t_: Change time     | Time     %`time
_a_: Change action   | Action   %`action

_c_: Create context
"
        ("n" (setq elc--context-name (read-from-minibuffer "Name: ")))
        ("l" (let ((gps (elc-get-gps)))
               (setq elc--context-location gps)
               (setq location (elc--gps-to-string gps))))
        ("t" (elc-create-timespan) :exit t)
        ("a" (setq elc--context-action (read-minibuffer "Action: ")))
        ("c" (progn
               (elc-add-context elc--context-name (ht (:action elc--context-action)
                                                      (:time elc--context-time)
                                                      (:location (ht (:gps elc--context-location)))))
               (setq elc--context-name "")
               (setq elc--context-location "")
               (setq elc--context-time "")
               (setq elc--context-action ""))
         :color blue))
      (hydra-context/body))))


(provide 'elcontext)

;;; elcontext.el ends here
