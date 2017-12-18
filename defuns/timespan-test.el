;;; timespan --- Handle timespans with elsip
;;; Commentary:
;;; Code:
(require 'ht)

(ert-deftest ts-within-timespanp ()
  "Test if a date is within a timespan."
  (should-error (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:from "12:00"))))
  (should-error (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:to "12:00"))))
  (should (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00"))))
  (should-not (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:from "14:00") (:to "15:00"))))
  (should (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:days '("Fri")))))
  (should-not (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:days '("Mon")))))
  (should (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00") (:days '("Mon" "Fri")))))
  (should (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:from "13:00") (:to "13:10") (:days '("Fri")))))
  (should (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:from "13:00") (:to "13:10"))))
  (should-not (ts-within-timespan (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00") (:days '("Mon" "Tue"))))))

;;; timespan-test.el ends here
