;;; elcontext --- Define context specific services in emacs

;; Copyright (C) 2018 Thomas Sojka

;; Author: Thomas Sojka

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ht)
(require 'deferred)
(require 'elcontext-utils)

(setq elcontext-location--current (ht))

(defun elcontext-location-edit (&optional lat lon)
  "Edit coordinates manually. Initial LAT and LON can be specified."
  (let ((lat (elcontext-utils-read-number-range 0 90 "Latitude: " (number-to-string lat)))
        (lon (elcontext-utils-read-number-range 0 90 "Longitude: " (number-to-string lon))))
    (ht (:lat (string-to-number lat)) (:lon (string-to-number lon)))))


(defun elcontext-location-valid-context (context)
  "Check if the CONTEXT is valid for current location."
  (lexical-let ((gps (ht-get context :location)))
    (deferred:$
      (deferred:process "CoreLocationCLI")
      (deferred:nextc it 'elcontext-location--gps-to-ht)
      (deferred:nextc it (lambda (x) (elcontext-location--within-range x gps))))))

(defun elcontext-location--gps-to-ht (gps-output)
  "Convert the GPS-OUTPUT to a gps coordinate hashtable."
  (let* ((gps (s-split-words gps-output))
         (lat (concat (nth 0 gps) "." (nth 1 gps)))
         (lon (concat (nth 2 gps) "." (nth 3 gps))))
    (ht (:lat (string-to-number lat)) (:lon (string-to-number lon)))))

(defun elcontext-location--within-range (gps1 gps2)
  "Ensure a GPS1 and GPS2 are wihtin 100 meters."
  (< (elcontext-location--distance gps1 gps2) 0.100))

(defun elcontext-location-get-gps ()
  "Return the current gps."
  (with-temp-buffer ()
                    (condition-case err
                        (progn
                          (call-process "CoreLocationCLI" nil t)
                          (elcontext-location--gps-to-ht (buffer-string)))
                      (file-error
                       (if (y-or-n-p "CoreLocationCLI not found. Do you want to download it? ")
                           (progn (browse-url "https://github.com/fulldecent/corelocationcli")
                                  (user-error ""))
                         (user-error "Please download https://github.com/fulldecent/corelocationcli to use el-context"))))))

(defun elcontext-location--distance (from to)
  "Comutes the distance between FROM and TO in km."
  (let ((earth-radius 6371))
    (* 2 earth-radius (asin (sqrt (elcontext-location--haversine from to))))))

(defun elcontext-location--haversine (from to)
  "Computes the haversine for two coordinates FROM and TO."
  (let ((dLat (elcontext-location--degree-to-radians (abs (- (ht-get to :lat) (ht-get from :lat)))))
        (dLon (elcontext-location--degree-to-radians (abs (- (ht-get to :lon) (ht-get from :lon)))))
        (lat1 (elcontext-location--degree-to-radians (ht-get from :lat)))
        (lat2 (elcontext-location--degree-to-radians (ht-get to :lat))))
    (+ (expt (sin (/ dLat 2)) 2) (* (cos lat1) (cos lat2) (expt (sin (/ dLon 2)) 2)))))

(defun elcontext-location--degree-to-radians (degrees)
  "Convert DEGREES to radians."
  (/ (* degrees pi) 180))

(defhydra elcontext-location-hydra (:hint nil :foreign-keys warn)
  "
_l_: Current location | %(elcontext-location-to-string (ht (:location elcontext-location--current)))
_e_: Edit location    |

_c_: Create location
_q_: Quit
"
  ("l" (setq elcontext-location--current (elcontext-location-get-gps)))
  ("e" (setq elcontext-location--current (elcontext-location-edit (ht-get elcontext-location--current :lat)
                                                      (ht-get elcontext-location--current :lon))))
  ("c" (progn
         (ht-set! elcontext--context-current :location elcontext-location--current)
         (setq elcontext-location--current (ht))
         (elcontext-hydra-create-context/body)) :exit t)
  ("q" (elcontext-hydra-create-context/body) :exit t))


(defun elcontext-location-create (context)
  "Create a new location or a edit a existing CONTEXT location from user input."
  (setq elcontext-location--current (ht-get context :location))
  (elcontext-location-hydra/body))

(defun elcontext-location-to-string (context)
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
