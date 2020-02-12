(in-package :celwk)

(defparameter +day-names+
  '("Monday"
    "Tuesday"
    "Wednesday"
    "Thursday"
    "Friday"
    "Saturday"
    "Sunday"))
;; "~10a"  => padding left with Spaces
;; "~10@a" => padding right

(defvar max-length (apply #'max (mapcar #'length +day-names+)))
(defun local-time (second minute hour date month year day-of-week dst-p tz)
  (declare (ignore dst-p))
  (input "~2,'0d:~2,'0d:~2,'0d of ~a, ~2,'0d/~2,'0d/~d (GMT~@d)"
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
(defun now ()
  (apply #'local-time (multiple-value-list (get-decoded-time))))
