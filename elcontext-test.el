;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:
(require 'ht)

(require 'elcontext-time-test)
(require 'elcontext-location-test)
(require 'elcontext-action-test)

(ert-deftest elcontext-store-context-test ()
  "Store a new location")

;;; elcontext-test.el ends here
