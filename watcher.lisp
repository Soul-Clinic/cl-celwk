(in-package :celwk)
(use-package :sb-thread)

(defparameter *watcher* (make-hash-table :test #'equal) "File watcher")
(defvar *last-watching* nil)

(defun watch-file (file callback &key (delay 0.1) (init nil))
  "Watch the file modifying."
  
  
  (unwatch-file file)
  (let* ((modify-time (and init 0))
         (thread (make-thread
                  (lambda()
                    (loop
                      (cond ((probe-file file)
                             (let1 (last (file-write-date file))
                               (ifnil! modify-time last)
                               (unless (= modify-time last)
                                 (apply callback
                                        file
                                        (when (= (count-args callback) 2)
                                          (list last)))
                                 (setf modify-time last))))
                            (:else
                             (setf modify-time -1)))
                      (sleep delay))))))
    (when (gethash file *watcher*)
      (remhash file *watcher*))
    (setf *last-watching* file
          (gethash file *watcher*) thread)))


(defun unwatch-file (&optional (file *last-watching*))
  (let ((watcher (gethash file *watcher*)))
    (when (and watcher (thread-alive-p watcher))
      (terminate-thread watcher))
    (remhash file *watcher*)))


