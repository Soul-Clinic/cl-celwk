#| http://docs.com/SBCL.htm

13.5 Semaphores => Liek JS: Promise.all(...) wait for all finished...
(wait-on-semaphore semaphore :n 5 :timeout 1)

14 	 Timers	=> setTimeout/setInterval without make a new sleep thread

(schedule-timer (make-timer (^()
(write-line "Hello, world")
(force-output)))
2)

(require :sb-concurrency)
17.2 sb-concurrency
17.2.3 Gates

|#
(in-package :celwk)
;; (require :sb-concurrency)
;; (use-package :sb-concurrency)
;; (use-package :sb-thread)

(defun race (fns &optional (timeout nil))
  (let ($threads
        data
        (gate (make-gate)))
    (dolist (fn fns)
      (push (make-thread
             (^()
               (setf data (call fn))
               (open-gate gate)))
            $threads))
    (if (wait-on-gate gate :timeout timeout)
        (output "~&Finish!!")
        (output "~&Timeout..."))
    (values data
            (mapc (^(thread)
                    (if (thread-alive-p thread)
                        (terminate-thread thread)))
                  $threads))))

(defun limit-time-call (timeout fn &key default-value arguments)
  "Call a fn inside timeout.
Second value will be t if return normally, else(timeout) nil"
  (let ((thread (make-thread fn :arguments arguments)))
    (multiple-value-bind (value state)
        (join-thread thread :default nil :timeout timeout)
      (if (eql state :timeout)
          ;; (progn (terminate-thread thread)  ;; No need to terminate it, it can save to cache
          (values default-value nil)
          (values value t)))))

(defun search-threads (keyword)
  (filter λ(=> (slot-value _ 'sb-thread::name)
               (search keyword ~))
          (sb-thread::list-all-threads)))

