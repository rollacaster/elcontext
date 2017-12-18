;;; timespan --- Handle timespans with elisp
;;; Commentary:
;;; Code:
(require 'calendar)
(require 'ht)
(require 'dash)

(defun ts--time-to-calendardate (date)
    "Convert time to calendar DATE."
    (let ((month (nth 4  date))
          (day (nth 3  date))
          (year (nth 5  date)))
      (substring (calendar-day-name (cons month (cons day (cons year ())))) 0 3)))

(defun ts--get-hour (timespan time)
  "Get the hour from a TIMESPAN for a certain TIME."
  (condition-case nil
      (car (mapcar 'string-to-number (split-string (ht-get* timespan :time time) ":")))
    (wrong-type-argument nil)))

(defun ts--get-minute (timespan time)
  "Get the minute from a TIMESPAN for a certain TIME."
  (condition-case nil
      (-last-item (mapcar 'string-to-number (split-string (ht-get* timespan :time time) ":")))
    (wrong-type-argument nil)))

(defun ts-within-timespan (date timespan)
  "Check if a DATE is within a TIMESPAN."
  (let ((hour (nth 2  (decode-time (date-to-time date))))
        (minute (nth 1  (decode-time (date-to-time date))))
        (day (ts--time-to-calendardate (decode-time (date-to-time date))))
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
     ((and (member day days) (> hour fromHour) (< hour toHour)) t))))

(provide 'timespan)

;;; timespan.el ends here
