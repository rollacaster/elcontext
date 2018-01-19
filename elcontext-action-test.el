;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)

(ert-deftest elc--mark-as-run-test ()
    "Mark the last time a context was run."
    (should (equal (ht-get (elc--mark-as-run (ht) '(23131 49480 398032 0)) :last-run)
                   (ht-get (ht (:last-run '(23131 49480 398032 0))) :last-run))))

(ert-deftest elc-action--same-day-test ()
  "Check if two times have the same day."
  (should (elc-action--same-day
           (date-to-time "2018-01-14 13:00")
           (date-to-time "2018-01-14 00:00")))
  (should-not (elc-action--same-day
               (date-to-time "2018-01-13 13:00")
               (date-to-time "2018-01-14 00:00"))))

(ert-deftest elc-action-valid-context ()
  "Should be nil if last-run is not defined."
  (should-not (elc-action-valid-context (ht))))

;;; elcontext-action-test.el ends here
