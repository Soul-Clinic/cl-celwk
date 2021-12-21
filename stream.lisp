(in-package :celwk)
(use-package :sb-thread)
(defmacro inspect-file ((var filename &key (byes '(bye end over)) (wait 0.3)) &body body)
  "Keep read a file, will invoke BODY when content added, one line a time.
 End when input keyword in BYES"
  (with-gensyms (bye)
    `(let (,bye)
       (make-thread (^()
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




