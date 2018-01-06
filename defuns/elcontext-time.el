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

(defun ts-timespan-to-string (timespan)
  "Format a TIMESPAN to a string."
  (let ((from-hour (ts--get-hour timespan :from))
        (from-minute (ts--get-minute timespan :from))
        (to-hour (ts--get-hour timespan :to))
        (to-minute (ts--get-minute timespan :to))
        (days (ht-get timespan :days)))
    (concat
     (when (not (null from-hour))
         (concat
          (ts-pad-time from-hour) ":" (ts-pad-time from-minute)
          "-"
          (ts-pad-time to-hour) ":" (ts-pad-time to-minute)))
     (when (not (null days))
         (concat (when (not (null from-hour)) " ")
                 (s-replace " " "," (s-replace ")" "" (s-replace "(" "" (format "%s" days)))))))))

(defun ts-pad-time (time)
  "Pad a TIME with leading 0s."
  (s-pad-left 2 "0" (number-to-string time)))

(provide 'elcontext-time)

;;; elcontext-time.el ends here
