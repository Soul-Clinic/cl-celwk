(in-package :celwk)

(defun shell (cmd &key (output *standard-output*))
    #+clisp
        (let ((str (ext:run-shell-command cmd :output:stream)))
            (loop for line = (read-line str nil)
             until (null line)
             do (print line)))
    #+ecl 
        (si:system cmd)
    #+sbcl 
    ;; (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*)
    (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output output :search t)
    #+clozure 
        (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))

(defun cmd (cmd &optional (output t))
  "Use uiop:run-program"           
  (uiop:run-program cmd :output output :ignore-error-status t))
;; Set output to '(:string :stripped t) as string


(defun weplant (sql)
    (sb-ext:run-program "/usr/local/mysql/bin/mysql" `("-uroot" "weplant2" "-e" ,sql) :input nil :output *standard-output*))


(defun psql (sql)
    (sb-ext:run-program "/Library/PostgreSQL/11/bin/psql" `("-c" ,sql) :input nil :output *standard-output*))


;; SHELL environment:
;; WHO="Savior Can" sbcl
;; *
;; * (posix-getenv "WHO") => "Savior Can"
;; (sb-ext:posix-environ) . (sb-ext:posix-getenv name)	

(defun program-stream (program &optional args)
  (let ((process (sb-ext:run-program program args
                                     :input :stream
                                     :output :stream
                                     :wait nil
                                     :search t)))
    (when process
      (make-two-way-stream (sb-ext:process-output process)
                           (sb-ext:process-input process)))))

;; CL-USER> (defparameter *stream* (program-stream "bc"))	;; basic calculator
;; *STREAM*
;; CL-USER> (format *stream* "5 * 6 + 9")
;; NIL
;; CL-USER> (finish-output *stream*)       ; will hang without this
;; NIL
;; CL-USER> (read-line *stream*)
;; "39"
;; NIL
;; CL-USER> (close *stream*)
;; T

