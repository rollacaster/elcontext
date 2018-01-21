;;; timespan --- Handle timespans with elsip -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ht)
(require 'elcontext-time)

(ert-deftest elcontext-time--within-timespanp ()
  "Test if a date is within a timespan."
  (should-error (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00"))))
  (should-error (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:to "12:00"))))
  (should (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00"))))
  (should-not (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "14:00") (:to "15:00"))))
  (should (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:days '("Fri")))))
  (should-not (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:days '("Mon")))))
  (should (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00") (:days '("Mon" "Fri")))))
  (should (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "13:00") (:to "13:10") (:days '("Fri")))))
  (should (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "13:00") (:to "13:10"))))
  (should-not (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:00") (ht (:from "12:00") (:to "15:00") (:days '("Mon" "Tue")))))
  (should (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:10") (ht (:from "13:00") (:to "14:00"))))
  (should (elcontext-time--within-timespanp (date-to-time "2017-12-01 13:10") (ht (:days '("Fri")) (:from "13:00") (:to "14:00")))))

(ert-deftest elcontext-time-timespan-to-string ()
    "Test timespan conversion."
    (should (equal (elcontext-time-to-string (ht (:time (ht (:from "13:00") (:to "13:10"))))) "13:00-13:10"))
    (should (equal (elcontext-time-to-string (ht (:time (ht (:from "13:00") (:to "13:10") (:days '("Mon" "Tue")))))) "13:00-13:10 Mon,Tue"))
    (should (equal (elcontext-time-to-string (ht (:time (ht (:from "13:00") (:to "13:10") (:days '("Mon")))))) "13:00-13:10 Mon"))
    (should (equal (elcontext-time-to-string (ht (:time (ht (:days '("Tue")))))) "Tue"))
    (should (equal (elcontext-time-to-string (ht (:time (ht (:days '("Mon" "Tue")))))) "Mon,Tue")))

(provide 'elcontext-time-test)

;;; elcontext-time-test.el ends here
