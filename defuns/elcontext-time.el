;;; timespan --- Handle timespans with elisp
;;; Commentary:
;;; Code:
(require 'calendar)
(require 'ht)
(require 'dash)
(require 's)
(require 'ivy)

(defun elc-time-date-to-calendardate (date)
    "Convert time to calendar DATE."
    (let ((month (nth 4  date))
          (day (nth 3  date))
          (year (nth 5  date)))
      (substring (calendar-day-name (cons month (cons day (cons year ())))) 0 3)))

(defun elc-time--get-hour (timespan time)
  "Get the hour from a TIMESPAN for a certain TIME."
  (condition-case nil
      (car (mapcar 'string-to-number (split-string (ht-get* timespan time) ":")))
    (wrong-type-argument nil)))

(defun elc-time--get-minute (timespan time)
  "Get the minute from a TIMESPAN for a certain TIME."
  (condition-case nil
      (-last-item (mapcar 'string-to-number (split-string (ht-get* timespan time) ":")))
    (wrong-type-argument nil)))

(defun elc-time-within-timespan (date timespan)
  "Check if a DATE is within a TIMESPAN."
  (let ((hour (nth 2  (decode-time date)))
        (minute (nth 1  (decode-time date)))
        (day (elc-time-date-to-calendardate (decode-time date)))
        (fromHour (elc-time--get-hour timespan :from))
        (fromMinute (elc-time--get-minute timespan :from))
        (toHour (elc-time--get-hour timespan :to))
        (toMinute (elc-time--get-minute timespan :to))
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

(defun elc-time-timespan-to-string (timespan)
  "Format a TIMESPAN to a string."
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
                 (s-replace " " "," (s-replace ")" "" (s-replace "(" "" (format "%s" days)))))))))

(defun elc-time--pad-time (time)
  "Pad a TIME with leading 0s."
  (s-pad-left 2 "0" (number-to-string time)))

(defvar elc-time--from "")
(defvar elc-time--to "")
(defvar elc-time--days ())
(defhydra hydra-timespan (:hint nil :foreign-keys warn)
    "
_f_: Change from | From %`elc-time--from
_t_: Change to   | To   %`elc-time--to
_d_: Add days    | Days %`elc-time--days
_r_: Remove days

_c_: Create timespan
"
    ("f" (let ((from-hour (s-pad-left 2 "0" (elc-time--read-hour)))
               (from-minute (s-pad-left 2 "0" (elc-time--read-minute))))
           (setq elc-time--from (concat from-hour ":" from-minute))
           (ht-set! elc--context-time :from (concat from-hour ":" from-minute))))
    ("t" (let ((to-hour (s-pad-left 2 "0" (elc-time--read-hour)))
               (to-minute (s-pad-left 2 "0" (elc-time--read-minute))))
           (setq elc-time--to (concat to-hour ":" to-minute))
           (ht-set! elc--context-time :to (concat to-hour ":" to-minute))))
    ("d" (progn
           (setq elc-time--days (-snoc elc-time--days (elc-time--read-week-days elc-time--days)))
           (ht-set! elc--context-time :days elc-time--days)))
    ("r" (let ((days (-remove-item (ivy-read "Remove day:" elc-time--days) elc-time--days)))
           (setq elc-time--days days)
           (ht-set! elc--context-time :days elc-time--days)))
    ("c" (progn
           (if (or (and (s-present? elc-time--from) (s-blank? elc-time--to))
                   (and (s-present? elc-time--to) (s-blank? elc-time--from)))
               (progn
                 (message "Please specify a from and to time.")
                 (hydra-timespan/body))
             (progn
               (setq elc--context-time (elc-time-timespan-to-string elc--context-time))
               (setq elc-time--from "")
               (setq elc-time--to "")
               (setq elc-time--days ())
               (hydra-context/body))))
     :color blue))
(defun elc-time-create-timespan ()
  "Create a new timespan from user input."
  (hydra-timespan/body))

(defun elc-time--read-number-range (from to prompt)
  "Read a number range between FROM and TO with a PROMPT."
  (let ((number))
    (while (not number)
      (let ((userInput (read-from-minibuffer prompt)))
        (if (and
             (s-numeric? userInput)
             (<= from (string-to-number userInput))
             (>= to (string-to-number userInput)))
            (setq number userInput)
          (read-from-minibuffer (format "Please specify a number between %d-%d." from to)))))
    number))

(defun elc-time--read-week-days (selected-days)
  "Read week days from user input ignoring SELECTED-DAYS."
  (ivy-read "Week day: "
            (-difference '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun" ) selected-days)
            :require-match t))

(defun elc-time--read-hour ()
  "Read an hour form user input."
  (elc-time--read-number-range 0 23 "Hour: "))

(defun elc-time--read-minute ()
  "Read an minute form user input."
  (elc-time--read-number-range 0 59 "Minute: "))

(provide 'elcontext-time)

;;; elcontext-time.el ends here
