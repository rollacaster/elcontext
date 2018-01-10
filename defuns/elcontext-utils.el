;;; elcontext --- Define context specific services in emacs
;;; Commentary:
;;; Code:

(defun elc-utils-read-number-range (from to prompt)
  "Read a number range between FROM and TO with a PROMPT."
  (let ((number))
    (while (not number)
      (let ((userInput (read-from-minibuffer prompt)))
        (if (and
             (<= from (string-to-number userInput))
             (>= to (string-to-number userInput)))
            (setq number userInput)
          (read-from-minibuffer (format "Please specify a number between %d-%d." from to)))))
    number))

(provide 'elcontext-utils)

;;; elcontext-utils.el ends here
