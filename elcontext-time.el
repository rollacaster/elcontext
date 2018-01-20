;;; timespan --- Handle timespans with elisp
;;; Commentary:
;;; Code:
(require 'calendar)
(require 'ht)
(require 'dash)
(require 's)
(require 'elcontext-utils)

(defun elc-time--date-to-calendardate (date)
    "Convert time to calendar DATE."
    (let ((month (nth 4  date))
          (day (nth 3  date))
          (year (nth 5  date)))
      (substring (calendar-day-name (cons month (cons day (cons year ())))) 0 3)))

(defun elc-time--get-hour (timespan time)
  "Get the hour from a TIMESPAN for a certain TIME."
  (condition-case nil
      (car (split-string (ht-get* timespan time) ":"))
    (wrong-type-argument nil)))

(defun elc-time--get-hour-number (timespan time)
  "Get the hour from a TIMESPAN for a certain TIME."
  (condition-case nil
      (string-to-number (elc-time--get-hour timespan time))
    (wrong-type-argument nil)))

(defun elc-time--get-minute (timespan time)
  "Get the minute from a TIMESPAN for a certain TIME."
  (condition-case nil
      (-last-item (split-string (ht-get* timespan time) ":"))
    (wrong-type-argument nil)))

(defun elc-time--get-minute-number (timespan time)
  "Get the minute from a TIMESPAN for a certain TIME."
  (condition-case nil
      (string-to-number (elc-time--get-minute timespan time))
    (wrong-type-argument nil)))

(defun elc-time--within-timespanp (date timespan)
  "Check if a DATE is within a TIMESPAN."
  (let ((hour (nth 2  (decode-time date)))
        (minute (nth 1  (decode-time date)))
        (day (elc-time--date-to-calendardate (decode-time date)))
        (fromHour (elc-time--get-hour-number timespan :from))
        (fromMinute (elc-time--get-minute-number timespan :from))
        (toHour (elc-time--get-hour-number timespan :to))
        (toMinute (elc-time--get-minute-number timespan :to))
        (days (ht-get timespan :days)))
    (cond
     ((and (numberp fromHour) (not (numberp toHour))) (user-error "From hour was specified without To hour"))
     ((and (numberp toHour) (not (numberp fromHour))) (user-error "From hour was specified without To hour"))
     ((and (member day days) (not (numberp fromHour)) (not (numberp toHour))) t)
     ((and (not (member day days)) (not (numberp fromHour)) (not (numberp toHour))) nil)
     ((and (null days) (> hour fromHour) (< hour toHour)) t)
     ((and (null days) (>= hour fromHour) (>= minute fromMinute) (< hour toHour)) t)
     ((and (null days) (>= hour fromHour) (>= minute fromMinute) (<= hour toHour) (<= minute toMinute)) t)
     ((and (member day days) (> hour fromHour) (< hour toHour)) t)
     ((and (member day days) (>= hour fromHour) (>= minute fromMinute) (< hour toHour)) t)
     ((and (member day days) (>= hour fromHour) (>= minute fromMinute) (<= hour toHour) (<= minute toMinute)) t))))

(defun elc-time--pad-time (time)
  "Pad a TIME with leading 0s."
  (s-pad-left 2 "0" time))

(defun elc-time--format-from (timespan)
  "Format the from hourso of a TIMESPAN."
  (ht-get timespan :from))

(setq elc-time--current (ht))

(defhydra elc-time-hydra (:hint nil :foreign-keys warn)
    "
_f_: Change from | From %(ht-get elc-time--current :from)
_t_: Change to   | To   %(ht-get elc-time--current :to)
_d_: Add days    | Days %(ht-get elc-time--current :days)
_r_: Remove days

_c_: Create timespan
_q_: Quit
"
    ("f" (let ((from-hour (elc-time--pad-time (elc-time--read-hour (elc-time--get-hour elc-time--current :from))))
               (from-minute (elc-time--pad-time (elc-time--read-minute (elc-time--get-minute elc-time--current :from)))))
           (ht-set! elc-time--current :from (concat from-hour ":" from-minute))))
    ("t" (let ((to-hour (elc-time--pad-time (elc-time--read-hour (elc-time--get-hour elc-time--current :to))))
               (to-minute (elc-time--pad-time (elc-time--read-minute (elc-time--get-minute elc-time--current :to)))))
           (ht-set! elc-time--current :to (concat to-hour ":" to-minute))))
    ("d" (ht-set! elc-time--current :days
                  (-snoc (ht-get elc-time--current :days)
                         (elc-time--read-week-days (ht-get elc-time--current :days)))))
    ("r" (ht-set! elc-time--current :days
                  (-remove-item (completing-read "Remove day:" (ht-get elc-time--current :days))
                                (ht-get elc-time--current :days))))
    ("c" (progn
           (if (or (and (s-present? (ht-get elc-time--current :from)) (s-blank? (ht-get elc-time--current :to)))
                   (and (s-present? (ht-get elc-time--current :to)) (s-blank? (ht-get elc-time--current :from))))
               (progn
                 (message "Please specify a from and to time.")
                 (elc-time-hydra/body))
             (progn
               (ht-set! elc--context-current :time elc-time--current)
               (setq elc-time--current (ht))
               (elc-hydra-create-context/body)))) :exit t)
    ("q" (progn
           (setq elc-time--current (ht))
           (ht-set! elc--context-current :time (ht))
           (elc-hydra-create-context/body)) :exit t))


(defun elc-time--read-week-days (selected-days)
  "Read week days from user input ignoring SELECTED-DAYS."
  (completing-read "Week day: "
            (-difference '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun" ) selected-days)
            :require-match t))

(defun elc-time--read-hour (&optional hour)
  "Read an hour form user input. Can define default HOUR."
  (elc-utils-read-number-range 0 23 "Hour: " hour))

(defun elc-time--read-minute (&optional minute)
  "Read an minute form user input. Can define default MINUTE."
  (elc-utils-read-number-range 0 59 "Minute: " minute))

(defun elc-time-create (context)
  "Create a new timespan or a edit a existing CONTEXT timespan from user input."
  (setq elc-time--current (ht-get context :time))
  (elc-time-hydra/body))

(defun elc-time-to-string (context)
  "Format a CONTEXT time to a string."
  (let ((timespan (ht-get context :time)))
    (if timespan
        (let ((from-hour (elc-time--get-hour timespan :from))
              (from-minute (elc-time--get-minute timespan :from))
              (to-hour (elc-time--get-hour timespan :to))
              (to-minute (elc-time--get-minute timespan :to))
              (days (ht-get timespan :days)))
          (concat
           (when (not (null from-hour))
             (concat
              (elc-time--pad-time from-hour) ":" (elc-time--pad-time from-minute)
              "-"
              (elc-time--pad-time to-hour) ":" (elc-time--pad-time to-minute)))
           (when (not (null days))
             (concat (when (not (null from-hour)) " ")
                     (s-replace " " "," (s-replace ")" "" (s-replace "(" "" (format "%s" days))))))))
      "")))

(defun elc-time-valid-context (context)
  "Check if the CONTEXT is valid for current time."
  (elc-time--within-timespanp (current-time) (ht-get context :time)))

(provide 'elcontext-time)

;;; elcontext-time.el ends here
