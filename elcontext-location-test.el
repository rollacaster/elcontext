;;; elcontext --- Define context specific services in emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ht)
(require 'elcontext-location)

(ert-deftest elcontext--get-gps-test ()
  "Get the lat/lon Coordinates"
  (let ((gps (elcontext-location-get-gps)))
    (should (numberp (ht-get gps :lon)))
    (should (numberp (ht-get gps :lat)))))

(ert-deftest elcontext--distance-test ()
  "Compute distance between two points"
  (let* ((coordA (ht (:lat 48.157262) (:lon 11.540225)))
         (coordB (ht (:lat 48.154438) (:lon 11.541286))))
    (should (= (elcontext-location--distance coordA coordB) 0.3237273177020497))))

(ert-deftest elcontext--gps-to-sting-test ()
  "Convert gps coordinat to a string."
  (should (equal (elcontext-location-to-string (ht (:location
                                              (ht (:lat 48.126011)
                                                  (:lon 11.558044)))))
                 "Lat: 48.126011 Lon: 11.558044")))

(provide 'elcontext-location-test)

;;; elcontext-location-test.el ends here
