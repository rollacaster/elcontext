;;; elcontext --- Define context specific services in emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ht)

(require 'elcontext-time-test)
(require 'elcontext-location-test)
(require 'elcontext-action-test)
(require 'elcontext-directory-test)

(ert-deftest elcontext-store-context-test ()
  "Store a new location")

;;; elcontext-test.el ends here
