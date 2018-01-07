;;; timespan --- Handle timespans with elsip
;;; Commentary:
;;; Code:
(require 'ht)
(require 'elcontext-time)

(ert-deftest elc-time-within-timespanp ()
  "Test if a date is within a timespan."
  (should-error (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00"))))
  (should-error (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:to "12:00"))))
  (should (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00"))))
  (should-not (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "14:00") (:to "15:00"))))
  (should (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:days '("Fri")))))
  (should-not (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:days '("Mon")))))
  (should (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00") (:days '("Mon" "Fri")))))
  (should (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "13:00") (:to "13:10") (:days '("Fri")))))
  (should (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "13:00") (:to "13:10"))))
  (should-not (elc-time-within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00") (:days '("Mon" "Tue")))))
  (should (elc-time-within-timespanp (date-to-time "2017-12-01 13:10") (ht (:from "13:00") (:to "14:00")))))

(ert-deftest elc-time-timespan-to-string ()
    "Test timespan conversion."
    (should (equal (elc-time-timespan-to-string (ht (:from "13:00") (:to "13:10"))) "13:00-13:10"))
    (should (equal (elc-time-timespan-to-string (ht (:from "13:00") (:to "13:10") (:days '("Mon" "Tue")))) "13:00-13:10 Mon,Tue"))
    (should (equal (elc-time-timespan-to-string (ht (:from "13:00") (:to "13:10") (:days '("Mon")))) "13:00-13:10 Mon"))
    (should (equal (elc-time-timespan-to-string (ht (:days '("Tue")))) "Tue"))
    (should (equal (elc-time-timespan-to-string (ht (:days '("Mon" "Tue")))) "Mon,Tue")))

;;; elcontext-time-test.el ends here
