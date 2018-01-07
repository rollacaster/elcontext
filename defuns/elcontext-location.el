;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:

(defun elc-location-get-gps ()
  "Return the current gps."
  (with-temp-buffer ()
    (call-process "whereami" nil t)
    (let ((lat (buffer-substring 11 20))
          (lon (buffer-substring 32 41)))
      (ht (:lat (string-to-number lat)) (:lon (string-to-number lon))))))

(defun elc-location-gps-to-string (gps)
  "Convert GPS coordinate in a readable format."
  (if gps
      (let ((lat (ht-get gps :lat))
            (lon (ht-get gps :lon)))
        (concat "Lat: " (number-to-string lat) " Lon: " (number-to-string lon)))
    ""))

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

(defun elc-location--get-lat (context)
  "Get the latitute for a CONTEXT."
  (ht-get* elc-contexts context :location :gps :lat))

(defun elc-location--get-lon (context)
  "Get the longitude for a CONTEXT."
  (ht-get* elc-contexts context :location :gps :lon))

(defun elc-location--degree-to-radians (degrees)
  "Convert DEGREES to radians."
  (/ (* degrees pi) 180))

;;; elcontext-location.el ends here
