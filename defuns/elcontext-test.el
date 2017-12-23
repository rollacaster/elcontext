;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)

(ert-deftest elc--get-gps-test ()
  "Get the lat/lon Coordinates"
  (let ((gps (elc--get-gps)))
    (should (numberp (ht-get gps :lon)))
    (should (numberp (ht-get gps :lat)))))

(ert-deftest elc--distance-test ()
  "Compute distance between two points"
  (let* ((coordA (ht (:lat 48.157262) (:lon 11.540225)))
         (coordB (ht (:lat 48.154438) (:lon 11.541286))))
    (should (= (elc--distance coordA coordB) 0.3237273177020497))))

(ert-deftest elc-store-context-test ()
  "Store a new location"
  (setq elc-contexts
        (ht ("Home"
             (ht (:location
                  (ht (:gps
                       (ht (:lat 48.155565)
                           (:lon 11.538645)))))))
            ))
  (elc-add-context "Office"
                   (ht (:location
                        (ht (:gps
                             (ht (:lat 48.126011)
                                 (:lon 11.558044)))))))
  (should (equal (elc--get-lat "Office") 48.126011))
  (should (equal (elc--get-lon "Office") 11.558044))
  (should (equal (elc--get-lat "Home") 48.155565))
  (should (equal (elc--get-lon "Home") 11.538645)))

(ert-deftest elc--gps-to-sting-test ()
  "Convert gps coordinat to a string."
  (should (equal (elc--gps-to-string (ht (:lat 48.126011)
                                         (:lon 11.558044)))
                 "Lat: 48.126011 Lon: 11.558044")))

;;; elcontext-test.el ends here
