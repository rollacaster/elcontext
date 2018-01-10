;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)

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

;;; elcontext-test.el ends here
