(in-package :celwk)

(defun files-with-suffix (suffixes &key dir (deep t))
  (when (atom suffixes)
    (setf suffixes (list suffixes)))
  (unless dir
    (error "Please set :dir"))	; TODO: then read it?
  (mapcan (^(suffix)
            (remove-if (^(path)
                         (or (eql 0 (position #\. (pathname-name path)))
                             (nil? (pathname-type path))))	; Ignore the private/hidden file
                       
                       (directory (concat (namestring dir)
                                          (if deep "/**/*." "/*.")
                                          (aesthetic suffix)))))
          suffixes))

(defun file-size (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (file-length in)))


(defun read-file (file &rest open-args)
  (unless (find :external-format open-args)
    (setf open-args (nconc '(:external-format :utf-8) open-args)))	;; default :utf-8 for emoji char
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
  (when readonly
    (cmd (input "chmod a-w ~a" file)))
  (values))


(defun org-code (code &key (type "lisp"))
  (format nil "#+BEGIN_SRC ~a ~%~a~%#+END_SRC~%" type code))

(defun build-orgs (&key dir (suffixes '("lisp" "cl")) (type "lisp") (deep t))
  "Note: the path DIR is relative to the path when executing."
  (with-output-to-string (stream)
    (let* ((dirs (group-by (compose~ #'string-capitalize #'last1 #'pathname-directory)
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

;; (defvar +default-path+ *default-pathname-defaults*)
;; (defun update-coding-org (&key (which "celwk") (to (namestring (upper-path +default-path+ 1))))
;;  (unless (search ".org" to)
;;    (setf to (concat to which ".org")))
;;   (write-to-file to (build-orgs :dir (merge-pathnames which +default-path+) :deep nil)))
;; (write-to-file "./recipes.org" (build-orgs :dir "~/Books/LISP Books/Common Lisp Recipes-code"))

(defun copy-file (from to)
  (let* ((element-type '(unsigned-byte 8))
         (buffer (make-array 8192 :element-type element-type)))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists :supersede)
        (loop (let ((position (read-sequence buffer in)))
                (when (zerop position)
                  (return))
                (write-sequence buffer out :end position)))
        (pathname out)))))


(defun file-suffix (filename)
  (string-downcase (last1 (split #/\./# filename))))




