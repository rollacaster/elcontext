;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)
(require 'prodigy)
(require 'timespan)
(require 'hydra)

(defvar elc-contexts (ht))

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
    (concat "Lat: " (number-to-string lat) " Lon: " (number-to-string) lon)))

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
                    (ts-within-timespan (current-time) (ht-get context :time)))
                   (progn
                     (funcall (ht-get context :action))
                     (message (concat "Run " name)))))
             elc-contexts)))
(setq elc--context-name "")
(setq elc--context-location "")
(setq elc--context-time "")
(setq elc--context-action "")
(defun elc-new-context ()
  "Create a new context."
  (interactive)
  (progn
    (setq elc--context-name (read-from-minibuffer "Name: "))
    (defhydra hydra-context (:hint nil)
      "
_n_: Change name     | Name     %`elc--context-name
_l_: Change location | Location %`elc--context-location
_t_: Change time     | Time     %`elc--context-time
_a_: Change action   | Action   %`elc--context-action
"
      ("n" (lambda ()
             (interactive)
             (setq elc--context-name (read-from-minibuffer "Name: "))))
      ("l" (lambda ()
             (interactive)
             (setq elc--context-location (elc--gps-to-string (elc--get-gps)))))
      ("t" (lambda ()
             (interactive)
             (setq elc--context-time (read-from-minibuffer "Time: "))))
      ("a" (lambda ()
             (interactive)
             (setq elc--context-action (read-from-minibuffer "Action: ")))))
    (hydra-context/body)))



(provide 'elcontext)

;;; elcontext.el ends here
