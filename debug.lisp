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

;; @defun => defun with both &rest & &key [functional.lisp]
(@defun $output (control-string &rest format-args &key (width 80))
  "($output `Hello, Coder ~a` `Can` :width 70)"
  (princ (input "~2&~a~%~a~&~2:*~a~%"
                (string-repeat "=" width)
                (apply #'format nil control-string format-args))))

(defmacro output+ (&rest vars)
  (let ((format (make-string-output-stream)))
    (dolist (_ vars)
      (write-string "~a:	~a~2%" format))
    `($output ,(get-output-stream-string format)
              ,@(mapcan λ`(',_ ,_) vars))))

;; (defun manifest (&key (port 6666))
;;   (manifest:start :port port))
