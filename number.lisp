(in-package :celwk)

(defun parse-float (string)
  (ignore-errors
    (let* ((dot (position #\. string))
           (istr (subseq string 0 dot))
           (dstr (if dot (subseq string (1+ dot)) "0"))
           (intg (if (empty-string? istr) 0 (parse-integer istr)))
           (deci (/ (parse-integer dstr) (expt 10 (length dstr)))))
      (+ intg deci 0.0))))        

(defun scope? (min max x)
  (when (> min max) (rotatef min max))
  (and (>= x min) (<= x max)))
