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
    (defhydra hydra-context (:hint nil
                                   :foreign-keys run)
      "
_n_: Change name     | Name     %`elc--context-name
_l_: Change location | Location %`elc--context-location
_t_: Change time     | Time     %`elc--context-time
_a_: Change action   | Action   %`elc--context-action

_c_: Create context
"
      ("n" (setq elc--context-name (read-from-minibuffer "Name: ")))
      ("l" (setq elc--context-location (elc--gps-to-string (elc--get-gps))))
      ("t" (elc-create-timespan) :exit t)
      ("a" (setq elc--context-action (read-minibuffer "Action: ")))
      ("c" (progn (setq elc--context-name "")
                 (setq elc--context-location "")
                 (setq elc--context-time "")
                 (setq elc--context-action ""))
       :color blue))
    (hydra-context/body)))

(defvar elc-from-hour "")
(defvar elc-from-minute "")
(defvar elc-to-hour "")
(defvar elc-to-minute "")
(defvar elc-days ())

(defhydra hydra-timespan (:hint nil :foreign-keys warn)
  "
_f_: Change from | From %`elc-from-hour : %`elc-from-minute
_t_: Change to   | To   %`elc-to-hour : %`elc-to-minute
_d_: Add days    | Days %`elc-days
_r_: Remove days

_c_: Create timespan
"
  ("f" (progn
         (setq elc-from-hour (s-pad-left 2 "0" (elc-read-hour)))
         (setq elc-from-minute (s-pad-left 2 "0" (elc-read-minute)))))
  ("t" (progn
         (setq elc-to-hour (s-pad-left 2 "0" (elc-read-hour)))
         (setq elc-to-minute (s-pad-left 2 "0" (elc-read-minute)))))
  ("d" (setq elc-days (-snoc elc-days (elc-read-week-days))))
  ("r" (setq elc-days (-remove-item (ivy-read "Remove day:" elc-days) elc-days)))
  ("c" (progn
         (if (or (and (s-present? elc-from-hour) (s-blank? elc-to-hour))
                 (and (s-present? elc-to-hour) (s-blank? elc-from-hour)))
             (progn
               (message "Please specify a from and to time.")
               (hydra-timespan/body))
           (progn
             (setq elc--context-time (elc-timespan-to-string))
             (progn (setq elc-from-hour "")
                    (setq elc-from-minute "")
                    (setq elc-to-hour "")
                    (setq elc-to-minute "")
                    (setq elc-days ()))
             (hydra-context/body))))
   :color blue))

(defun elc-timespan-to-string ()
  "Format a timespan to a string."
  (concat
   (if (s-present? elc-from-hour)
       (concat "From " elc-from-hour ":" elc-from-minute
               " To " elc-to-hour ":" elc-to-minute " ")
     "")
   (if (not (null elc-days))
       (concat "On " (s-replace " " ", " (s-replace ")" "" (s-replace "(" "" (format "%s" elc-days)))))
     "")))

(defun elc-create-timespan ()
  "Create a new timespan from user input."
  (interactive)
  (hydra-timespan/body))

(defun elc-read-number-range (from to prompt)
  "Read a number range between FROM and TO with a PROMPT."
  (let ((number))
    (while (not number)
      (let ((userInput (read-from-minibuffer prompt)))
        (if (and
             (s-numeric? userInput)
             (<= from (string-to-number userInput))
             (>= to (string-to-number userInput)))
            (setq number userInput)
          (read-from-minibuffer (format "Please specify a number between %d-%d." from to)))))
    number))

(defun elc-read-week-days ()
  "Read week days from user input."
  (ivy-read "Week day: "
            (-difference '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun" ) elc-days)
            :require-match t))

(defun elc-read-hour ()
  "Read an hour form user input."
  (elc-read-number-range 0 23 "Hour: "))

(defun elc-read-minute ()
  "Read an minute form user input."
  (elc-read-number-range 0 59 "Minute: "))

(provide 'elcontext)

;;; elcontext.el ends here
