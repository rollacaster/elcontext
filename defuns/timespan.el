;;; timespan --- Handle timespans with elisp
;;; Commentary:
;;; Code:
(require 'calendar)
(require 'ht)
(require 'dash)
(require 's)
(require 'ivy)

(defun ts--time-to-calendardate (date)
    "Convert time to calendar DATE."
    (let ((month (nth 4  date))
          (day (nth 3  date))
          (year (nth 5  date)))
      (substring (calendar-day-name (cons month (cons day (cons year ())))) 0 3)))

(defun ts--get-hour (timespan time)
  "Get the hour from a TIMESPAN for a certain TIME."
  (condition-case nil
      (car (mapcar 'string-to-number (split-string (ht-get* timespan time) ":")))
    (wrong-type-argument nil)))

(defun ts--get-minute (timespan time)
  "Get the minute from a TIMESPAN for a certain TIME."
  (condition-case nil
      (-last-item (mapcar 'string-to-number (split-string (ht-get* timespan time) ":")))
    (wrong-type-argument nil)))

(defun ts-within-timespan (date timespan)
  "Check if a DATE is within a TIMESPAN."
  (let ((hour (nth 2  (decode-time date)))
        (minute (nth 1  (decode-time date)))
        (day (ts--time-to-calendardate (decode-time date)))
        (fromHour (ts--get-hour timespan :from))
        (fromMinute (ts--get-minute timespan :from))
        (toHour (ts--get-hour timespan :to))
        (toMinute (ts--get-minute timespan :to))
        (days (ht-get timespan :days)))
    (cond
     ((and (numberp fromHour) (not (numberp toHour))) (user-error "From hour was specified without To hour"))
     ((and (numberp toHour) (not (numberp fromHour))) (user-error "From hour was specified without To hour"))
     ((and (member day days) (not (numberp fromHour)) (not (numberp toHour))) t)
     ((and (not (member day days)) (not (numberp fromHour)) (not (numberp toHour))) nil)
     ((and (null days) (> hour fromHour) (< hour toHour)) t)
     ((and (null days) (>= hour fromHour) (>= minute fromMinute) (<= hour toHour) (<= minute toMinute)) t)
     ((and (member day days) (> hour fromHour) (< hour toHour)) t)
     ((and (member day days) (>= hour fromHour) (>= minute fromMinute) (<= hour toHour) (<= minute toMinute)) t))))

(setq ts-from-hour "")
(setq ts-from-minute "")
(setq ts-to-hour "")
(setq ts-to-minute "")
(setq ts-days ())
(defun ts-create-timespan ()
  "Create a new timespan from user input."
  (interactive)
  (progn
    (defhydra hydra-timespan (:hint nil)
      "
_f_: Change from | From %`ts-from-hour : %`ts-from-minute
_t_: Change to   | To   %`ts-to-hour : %`ts-to-minute
_d_: Add days    | Days %`ts-days
_r_: Remove days
"
      ("f" (lambda ()
             (interactive)
             (setq ts-from-hour (ts-read-hour))
             (setq ts-from-minute (ts-read-minute))))
      ("t" (lambda ()
             (interactive)
             (setq ts-to-hour (ts-read-hour))
             (setq ts-to-minute (ts-read-minute))))
      ("d" (lambda ()
             (interactive)
             (setq ts-days (-snoc ts-days (ts-read-week-days)))))
      ("r" (lambda ()
             (interactive)
             (setq ts-days (-remove-item (ivy-read "Remove day:" ts-days) ts-days)))))
    (hydra-timespan/body)))

(defun ts-read-number-range (from to prompt)
  "Read a number range between FROM and TO with a PROMPT."
  (let ((number))
    (while (not number)
      (let ((userInput (read-from-minibuffer prompt)))
        (if (and
             (s-numeric? userInput)
             (<= from (string-to-number userInput))
             (>= to (string-to-number userInput)))
            (setq number (string-to-number userInput))
          (read-from-minibuffer (format "Please specify a number between %d-%d." from to)))))
    number))

(defun ts-read-week-days ()
  "Read week days from user input."
  (ivy-read "Week day: "
            '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun" )
            :require-match t))

(defun ts-read-hour ()
  "Read an hour form user input."
  (ts-read-number-range 0 23 "Hour: "))

(defun ts-read-minute ()
  "Read an minute form user input."
  (ts-read-number-range 0 59 "Minute: "))

(provide 'timespan)

;;; timespan.el ends here
