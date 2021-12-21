(in-package :celwk)

(defparameter +day-names+
  '("Monday"
    "Tuesday"
    "Wednesday"
    "Thursday"
    "Friday"
    "Saturday"
    "Sunday"))
(defparameter +day-names+
  '("星期一"
    "星期二"
    "星期三"
    "星期四"
    "星期五"
    "星期六"
    "星期天"))

;; "~10a"  => padding left with Spaces
;; "~10@a" => padding right

(defvar max-length (apply #'max (mapcar #'length +day-names+)))
(defun local-time (second minute hour date month year day-of-week dst-p tz)
  (declare (ignore dst-p))
  (input "~2,'0d:~2,'0d:~2,'0d ~a, ~2,'0d/~2,'0d/~d (GMT~@d)"
         hour
         minute
	     second
	     (input (concat "~" (write-to-string max-length) "@a") (nth day-of-week +day-names+))	;; padding right
         month
	     date
	     year
	     (- tz)))
;; time-zone 0
(defun time-of-universal (timestamp &key (from 1900))
  (apply #'local-time (multiple-value-list (decode-universal-time
                                            (+ timestamp (encode-universal-time 0 0 0 1 1 from 0))))))

(defun date-of-universal (timestamp &key (from 1970))
  (destructuring-bind (second minute hour date month year day-of-week dst-p tz)
      (multiple-value-list (decode-universal-time
                            (+ timestamp (encode-universal-time 0 0 0 1 1 from 0))))
    (declare (ignore second minute hour day-of-week dst-p tz))
    (input "~d-~2,'0d-~2,'0d"
	     year
         month
	     date)))

(defun now- ()
  (apply #'local-time (multiple-value-list (get-decoded-time))))

;; (get-internal-real-time)
(defun now ()
  (multiple-value-bind (second minute hour date month year day-of-week) (decode-universal-time (get-universal-time))
    (input "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
           year month date hour minute second)))

(defun now* ()
  (multiple-value-bind (second minute hour date month year day-of-week) (decode-universal-time (get-universal-time))
    (declare (ignore year day-of-week))
    (input "~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
           month date hour minute second)))

(defun pretty-time (&key date-only timestamp (week-day? t))
  "2020-05-03 14:23:58 星期天"
  (unless timestamp
    (setf timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day-of-week) (decode-universal-time timestamp)
    (if date-only
        (input "~d-~2,'0d-~2,'0d"
               year month date)
        (input "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a"
               year month date hour minute second (if week-day? (nth day-of-week +day-names+) "")))))


(defun only-time (&optional (timestamp (get-universal-time)))
  "14:23:58"
  (multiple-value-bind (second minute hour date month year day-of-week) (decode-universal-time timestamp)
    (input "~2,'0d:~2,'0d:~2,'0d"
           hour minute second)))


(defun machine ()
  (list (short-site-name)
        (long-site-name)
        (lisp-implementation-type)
        (lisp-implementation-version)
        (machine-instance)
        (machine-type)
        (machine-version)
        (software-type)
        (software-version)))

(defmacro toggle! (arg)
  `(setf ,arg (not ,arg)))

(defmacro !!! (arg)
  `(setf ,arg (not ,arg)))
