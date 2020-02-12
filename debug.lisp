(in-package :celwk)
(defmacro with-time (codes)
  `(progn
     (format *trace-output* "~2&~a" ',codes)
     (time ,codes)))

(defmacro io (fmt &rest args)
  (setf fmt (concat fmt "~&"))
  `(progn
     (output ,fmt ,@args)
     (input  ,fmt ,@args)))

;; defun! => defun with both &rest & &key [functional.lisp]
(defun! $output (control-string &rest format-args &key (width 80))
  "($output `Hello, Coder ~a` `Can` :width 70)"
  (output "~&~a~%~a~&~2:*~a~%"
         (string-repeat "=" width)
         (apply #'format nil control-string format-args)))

(defun manifest (&key (port 6666))
  (manifest:start :port port))
