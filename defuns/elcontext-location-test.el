;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:

(ert-deftest elc--get-gps-test ()
  "Get the lat/lon Coordinates"
  (let ((gps (elc-location-get-gps)))
    (should (numberp (ht-get gps :lon)))
    (should (numberp (ht-get gps :lat)))))

(ert-deftest elc--distance-test ()
  "Compute distance between two points"
  (let* ((coordA (ht (:lat 48.157262) (:lon 11.540225)))
         (coordB (ht (:lat 48.154438) (:lon 11.541286))))
    (should (= (elc-location--distance coordA coordB) 0.3237273177020497))))

(ert-deftest elc--gps-to-sting-test ()
  "Convert gps coordinat to a string."
  (should (equal (elc-location-to-string (ht (:lat 48.126011)
                                         (:lon 11.558044)))
                 "Lat: 48.126011 Lon: 11.558044")))

;;; elcontext-location-test.el ends here
