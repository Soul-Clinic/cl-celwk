(in-package :celwk)

(defun decimal (number count)
  "Approximate value with DECIMAL => (decimal pi 4) => 3.1416"
  (* 1.0 (/ (round number (expt 0.1 count))
            (expt 10 count))))
;; grep-find !! Useful!
(defun calculus (fn a b &key (power 3))
  "(calculus 'cos 0 pi) => 2.0"  
  (let* ((min (min a b))
         (max (max a b))
         (s 0)
         (unit (expt 10 (- power)))
         (x (+ min (/ unit 2))))
    (while (<= x max)
      (incf s (* unit (call fn x)))
      (incf x unit))
    (decimal s power)))

