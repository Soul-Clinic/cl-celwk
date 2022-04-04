(in-package :celwk)

(defun save-pid (&optional (path #/$(sb-posix:getcwd)/.pid/#))
  (let1 (pid (sb-posix:getpid))
    (write-to-file path (write-to-string pid))
    (format *error-output* "~%Saved :pid to ~A" path)
    pid))

(defun suicide (&optional (signal 3))	; -QUIT  `man kill`
  (sb-posix:kill (sb-posix:getpid) signal))


(defmacro set-signal-handler (signo &body body)
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
         (declare (ignore signo))
         ,@body)
       (cffi:foreign-funcall "signal" :int ,signo :pointer (cffi:callback ,handler)))))

(set-signal-handler sb-posix:sigio
  ($output "Accept Signal: sigio(~A)" sb-posix:sigio))

(set-signal-handler sb-posix:sigalrm
  ($output "Git Updated. signal:~A~%~A" sb-posix:sigalrm (cmd "git log -1")))

;; Put "kill -io `cat ~/Projects/celwk/.pid`" in git hooks
                    

;; (dotimes (i 32)
;;   (let1 (signal i)
;;     (set-signal-handler signal ;; Run in the current running thread
;;       (format t "Signal: ~d ~%" signal))))

;; (sb-posix:kill (sb-posix:getpid) 1)
;; (sb-unix:unix-kill (sb-posix:getpid) 2)

;; SIGHUP  1       /* hangup */
;; SIGINT  2       /* interrupt Ctrl+C */
;; SIGQUIT 3       /* quit Ctrl+\ */
;; SIGILL  4       /* illegal instruction (not reset when caught) */
;; SIGTRAP 5       /* trace trap (not reset when caught) */
;; SIGABRT 6       /* abort() */
;; SIGPOLL 7       /* pollable event ([XSR] generated, not supported) */
;; SIGIOT  SIGABRT /* compatibility */
;; SIGEMT  7       /* EMT instruction */
;; #endif  /* (!_POSIX_C_SOURCE || _DARWIN_C_SOURCE) */
;; SIGFPE  8       /* floating point exception */
;; SIGKILL 9       /* kill (cannot be caught or ignored) */
;; SIGBUS  10      /* bus error */
;; SIGSEGV 11      /* segmentation violation */
;; SIGSYS  12      /* bad argument to system call */
;; SIGPIPE 13      /* write on a pipe with no one to read it */
;; SIGALRM 14      /* alarm clock */
;; SIGTERM 15      /* software termination signal from kill */
;; SIGURG  16      /* urgent condition on IO channel */
;; SIGSTOP 17      /* sendable stop signal not from tty */
;; SIGTSTP 18      /* stop signal from tty */
;; SIGCONT 19      /* continue a stopped process */
;; SIGCHLD 20      /* to parent on child stop or exit */
;; SIGTTIN 21      /* to readers pgrp upon background tty read */
;; SIGTTOU 22      /* like TTIN for output if (tp->t_local&LTOSTOP) */

;; SIGIO   23      /* input/output possible signal */

;; SIGXCPU 24      /* exceeded CPU time limit */
;; SIGXFSZ 25      /* exceeded file size limit */
;; SIGVTALRM 26    /* virtual time alarm */
;; SIGPROF 27      /* profiling time alarm */

;; SIGWINCH 28     /* window size changes */
;; SIGINFO 29      /* information request */

;; SIGUSR1 30      /* user defined signal 1 */
;; SIGUSR2 31      /* user defined signal 2 */

;; SIG_DFL         (void (*)(int))0
;; SIG_IGN         (void (*)(int))1
;; SIG_HOLD        (void (*)(int))5
;; SIG_ERR         ((void (*)(int))-1)





