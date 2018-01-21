;;; elcontext --- Define context specific services in emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ht)

(ert-deftest elcontext--mark-as-run-test ()
    "Mark the last time a context was run."
    (should (equal (ht-get (elcontext--mark-as-run (ht) '(23131 49480 398032 0)) :last-run)
                   (ht-get (ht (:last-run '(23131 49480 398032 0))) :last-run))))

(ert-deftest elcontext-action--same-day-test ()
  "Check if two times have the same day."
  (should (elcontext-action--same-day
           (date-to-time "2018-01-14 13:00")
           (date-to-time "2018-01-14 00:00")))
  (should-not (elcontext-action--same-day
               (date-to-time "2018-01-13 13:00")
               (date-to-time "2018-01-14 00:00"))))

(ert-deftest elcontext-action-valid-context ()
  "Should be t if last-run is not defined."
  (should (elcontext-action-valid-context (ht))))

(provide 'elcontext-action-test)

;;; elcontext-action-test.el ends here
