;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)
(require 'deferred)
(require 'elcontext-utils)

(setq elc-location--current (ht))

(defun elc-location-edit (&optional lat lon)
  "Edit coordinates manually. Initial LAT and LON can be specified."
  (let ((lat (elc-utils-read-number-range 0 90 "Latitude: " (number-to-string lat)))
        (lon (elc-utils-read-number-range 0 90 "Longitude: " (number-to-string lon))))
    (ht (:lat (string-to-number lat)) (:lon (string-to-number lon)))))


(defun elc-location-valid-context (context)
  "Check if the CONTEXT is valid for current location."
  (lexical-let ((gps (ht-get context :location)))
    (deferred:$
      (deferred:process "CoreLocationCLI")
      (deferred:nextc it 'elc-location--gps-to-ht)
      (deferred:nextc it (lambda (x) (elc-location--within-range x gps))))))

(defun elc-location--gps-to-ht (gps-output)
  "Convert the GPS-OUTPUT to a gps coordinate hashtable."
  (let* ((gps (s-split-words gps-output))
         (lat (concat (nth 0 gps) "." (nth 1 gps)))
         (lon (concat (nth 2 gps) "." (nth 3 gps))))
    (ht (:lat (string-to-number lat)) (:lon (string-to-number lon)))))

(defun elc-location--within-range (gps1 gps2)
  "Ensure a GPS1 and GPS2 are wihtin 100 meters."
  (< (elc-location--distance gps1 gps2) 0.100))

(defun elc-location-get-gps ()
  "Return the current gps."
  (with-temp-buffer ()
                    (condition-case err
                        (progn
                          (call-process "CoreLocationCLI" nil t)
                          (elc-location--gps-to-ht (buffer-string)))
                      (file-error
                       (if (y-or-n-p "CoreLocationCLI not found. Do you want to download it? ")
                           (progn (browse-url "https://github.com/fulldecent/corelocationcli")
                                  (user-error ""))
                         (user-error "Please download https://github.com/fulldecent/corelocationcli to use el-context"))))))

(defun elc-location--distance (from to)
  "Comutes the distance between FROM and TO in km."
  (let ((earth-radius 6371))
    (* 2 earth-radius (asin (sqrt (elc-location--haversine from to))))))

(defun elc-location--haversine (from to)
  "Computes the haversine for two coordinates FROM and TO."
  (let ((dLat (elc-location--degree-to-radians (abs (- (ht-get to :lat) (ht-get from :lat)))))
        (dLon (elc-location--degree-to-radians (abs (- (ht-get to :lon) (ht-get from :lon)))))
        (lat1 (elc-location--degree-to-radians (ht-get from :lat)))
        (lat2 (elc-location--degree-to-radians (ht-get to :lat))))
    (+ (expt (sin (/ dLat 2)) 2) (* (cos lat1) (cos lat2) (expt (sin (/ dLon 2)) 2)))))

(defun elc-location--degree-to-radians (degrees)
  "Convert DEGREES to radians."
  (/ (* degrees pi) 180))

(defhydra elc-location-hydra (:hint nil :foreign-keys warn)
  "
_l_: Current location | %(elc-location-to-string (ht (:location elc-location--current)))
_e_: Edit location    |

_c_: Create location
_q_: Quit
"
  ("l" (setq elc-location--current (elc-location-get-gps)))
  ("e" (setq elc-location--current (elc-location-edit (ht-get elc-location--current :lat)
                                                      (ht-get elc-location--current :lon))))
  ("c" (progn
         (ht-set! elc--context-current :location elc-location--current)
         (setq elc-location--current (ht))
         (elc-hydra-create-context/body)) :exit t)
  ("q" (elc-hydra-create-context/body) :exit t))


(defun elc-location-create (context)
  "Create a new location or a edit a existing CONTEXT location from user input."
  (setq elc-location--current (ht-get context :location))
  (elc-location-hydra/body))

(defun elc-location-to-string (context)
  "Format a CONTEXT location to a string."
  (let ((gps (ht-get context :location)))
    (if gps
        (let ((lat (ht-get gps :lat))
              (lon (ht-get gps :lon)))
          (if (and lat lon)
            (concat "Lat: " (number-to-string lat) " Lon: " (number-to-string lon))
            ""))
      "")))

(provide 'elcontext-location)

;;; elcontext-location.el ends here
