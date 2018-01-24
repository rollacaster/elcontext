;;; elcontext --- Define context specific services in emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ht)

(ert-deftest elcontext-directory-valid-context ()
  "Test if directory context is valid"
  (should (equal (elcontext-directory-valid-context (ht (:directory default-directory))) t))
  (should (equal (elcontext-directory-valid-context (ht)) t)))

(provide 'elcontext-directory-test)

;;; elcontext-directory-test.el ends here
