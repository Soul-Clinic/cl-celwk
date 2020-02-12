(in-package :celwk)


(defun files-with-suffix (suffixes &key dir deep)
  (when (atom suffixes)
    (setf suffixes (list suffixes)))
  (unless dir
    (error "Please set :dir"))	; TODO: then read it?
  (mapcan (^(suffix)
            (remove-if (^(path)
                         (position #\. (pathname-name path)))	; (file-namestring path)
                       (directory (concat (namestring dir)
                                          (if deep "/**/*." "/*.")
                                          suffix))))
          suffixes))

(defun file-size (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (file-length in)))


(defun read-file (file &rest open-args)
  (with-open-stream (stream (apply 'open file open-args))
    (let* ((buffer (make-array (file-length stream)
                               :fill-pointer t
                               :element-type (stream-element-type stream)))
           (position (read-sequence buffer stream)))
      (setf (fill-pointer buffer) position)	;; Remove the end irrelevant chars
      buffer)))

(defun write-to-file (file data &key readonly)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sequence data stream))
  (if readonly (cmd (input "chmod a-w ~a" file)))
  (values))


(defun org-code (code &key (type "lisp"))
  (format nil "#+BEGIN_SRC ~a ~%~a~%#+END_SRC~%" type code))

(defun build-orgs (&key dir (suffixes '("lisp" "cl")) (type "lisp") (deep t))
  "Note: the path DIR is relative to the path when executing."
  (with-output-to-string (stream)
    (let* ((dirs (group-by (compose~ #'string-capitalize #'last* #'pathname-directory)
                           (files-with-suffix suffixes :dir dir :deep deep)
                           :out #'cons))
           (paths (sort dirs
                        (^(d1 d2)	;; directory/folder
                          (string< (car d1) (car d2))))))
      (mapcan (^(folder)
                (let ((folder-name (car folder))
                      (files (reverse (cdr folder))))
                  (format stream "~&* ~a~%~{** [[~a][~a]]~%~a~%~}"
                          folder-name
                          (mapcan (^(file)
                                    (list file
                                          (pathname-name file)
                                          (org-code (read-file file) :type type)))
                                  files))))                        
              paths))))

(defun upper-path (path &optional (level 1))
  (truename (concat (namestring path) (join (repeat level "..") :seperator "/"))))
  
(defun update-coding-org (&key (which "celwk") (to (namestring (upper-path +default-path+ 1))))
  (unless (search ".org" to)
    (setf to (concat to which ".org")))
  (write-to-file to (build-orgs :dir (merge-pathnames which +default-path+) :deep nil)))
;; (time (update-coding-org :which "celwk"))
;; (time (update-coding-org :which "server"))
#|
(defun merge-codes-to-orgs (filename &key dir suffixes type deep readonly)
  (write-to-file filename
                 (build-orgs :dir dir
                             :suffixes suffixes
                             :type type
                             :deep deep)
                 :readonly readonly))
|#
;; (write-to-file "./recipes.org" (build-orgs :dir "~/Books/LISP Books/Common Lisp Recipes-code"))
