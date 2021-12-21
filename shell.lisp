(in-package :celwk)

;; (ql:quickload :external-program)
;; (external-program:run "pwd" nil :output *standard-output*)
;; (time (string-trim (string #\Newline) (with-output-to-string (out)
;;           (external-program:run "date" nil :output out))))

(defun shell (cmd &key (output *standard-output*) (wait t))
  #+clisp
  (let ((str (ext:run-shell-command cmd :output:stream)))
    (loop for line = (read-line str nil)
       until (null line)
       do (print line)))
  #+ecl 
  (si:system cmd)
  #+sbcl 
  ;; (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*)
  (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output output :search t :wait wait :external-format '(:utf-8 :replacement #\?))
  ;; '(:utf-8 :replacement #\?) for Special Symbol can't analyse
  
  #+clozure 
  (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))
(<=> shell run)

;; (with-open-stream (*standard-output* (make-broadcast-stream))		; Mute the output
;;   (shell "scp ~/Lisp/mine/house/server/settings.json al:~/Lisp/mine/house/server/settings.json"))

(defvar +newline+ (input "~%"))

;; (defun shell-input (cmd)
;;   (>> (with-output-to-string (x)
;;         (shell cmd :output x))
;;       (string-trim +newline+ ~)
;;       (time ~)))

(defun shell-input (cmd)
  (<< (time ~)
      (string-trim +newline+ ~)
      (with-output-to-string (x)
        (shell cmd :output x))))



(defun cmd (cmd &optional (output '(:string :stripped t)) (error *error-output*))
  "Use uiop:run-program"           
  (uiop:run-program cmd :output output :error-output error :ignore-error-status t))
;; Set output to '(:string :stripped t) as string, to t as *standard-output*


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

