(in-package :celwk)
(defmacro inspect-file ((var filename &key (byes '(bye end over)) (wait 0.3)) &body body)
  "Keep read a file, will invoke BODY when content added, one line a time.
 End when input keyword in BYES"
  (with-gensyms (bye)
    `(let (,bye)
       (sb-thread:make-thread (^()
                                ;; Wait for the byes keyword to end
                                ;; #.*standard-input* => eval at Compile time:
                                ;; #.(get-universal-time) will not change even inside a defun
                                ;; BUT, only return 1 value, multiple-value-list not work
                                (until (member (read *standard-input*) ',byes))                               
                                (setf ,bye t)))
       (with-open-file (fifo ,filename :if-does-not-exist :create)
         (do ((text ,@(repeat 2 '(read-line fifo nil nil))))
         ;; (do ((text 1 2)) ;;(read-line fifo nil nil) (read-line fifo nil nil)))
             ;; => ((text (read-line fifo nil nil) (read-line fifo nil nil)))
             (,bye)
           (if text 
               (let ((,var text))
                 ,@body)
               (sleep ,wait)))))))

#|
(defmacro keep-reading ((var filename &key (byes '(bye end over)) (wait 0.3)) &body body)
  "Keep read a file, will invoke body when content added"
  (with-gensyms (bye input output)
    `(let (,bye
           (,input *standard-input*)
           (,output *standard-output*))
       (sb-thread:make-thread #'(lambda (standard-input)
                                  (until 
                                   (member (read standard-input) ,byes)) ;; Or use ,input to replace another-input
                                  (setf ,bye t)
                                  (format ,output "Goodbye ~%")
                                  (princ "Bye!!"))
                              :arguments (list *standard-input*))
       (with-open-file (fifo ,filename :if-does-not-exist :create)
         (loop 
            (let ((text (read-line fifo nil nil)))
              (when ,bye
                (format t "~&It's over, guy ~%")
                (return (format nil "Goodbye ~a" (now))))
              (if text
                  (let ((,var text))
                    ,@body)
                  (sleep ,wait))))))))

|#

;; (inspect-file (text "stream.lisp" :byes (bye love)) 
;;              (output "~a: ~a~%" (now) text))
