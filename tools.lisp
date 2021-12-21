(in-package :celwk)
;;; Example of use:  (ppmx (incf a))
;; (load "make-notes.lisp")
(defmacro ppmx (form &optional (count 30))
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	      (exp (macroexpand exp1))
	      (*print-circle* nil))
     (cond ((equal exp exp1)
	        (output "~&Macro expansion: ~%~A" (wrapped-code exp "*" ,count)))
	       (t (output "~&First step of expansion:")
	          (pprint exp1)
	          (output "~%~%Final expansion:")
	          (pprint exp)))
     (output "~%~%")
     (values)))
(alias pmx ppmx)


(defmacro bound-type (name)
  "(values :value :function)"
  `(cond
     ((boundp  ',name) (funcall #'values :value (when (fboundp ',name) :function)))
     ((fboundp ',name) (values :function nil))
     (t (values nil nil))))

(defparameter *debug?* t)
(defmacro vprint (&rest vars)
  "(vprint a b x):
 a: **
 b: **
 x: **"
  `(and *debug?* (output "~&~%~@{~A: 	~S~%~}~%"
                         ,@(mapcan (^(x) (if (string? x) (list ":" x) `(',x ,x))) vars))))

(alias vp vprint)

(defun 2d-array-to-list (array)
  (map 'list #'identity array))


(defun code-define-list (file &optional (stream *standard-output*))
  "List all defun/demacro for package"
  (format stream "~&;; => ~a~%" (string-capitalize (pathname-name file)))
  ;; (vprint (pathname? file))
  (unless (or (pathname? file)
              (find #\. file))
    (setf file (concat file ".lisp")))
  (mapc (^(def)
          (setf def (string-of-code def))
          (let* ((code (read-file file))
                 (reg (input "\\n {0,4}\\(~a ([^, '`]+) " def))
                 (matches (all-matches-as-strings reg code)))
            (format stream "~&~{#:~a~%~}~%"
                    (mapcan $(2d-array-to-list (second-value (scan-to-strings reg $1)))
                            matches))))
        '(defun defmacro)))

(defun export-all (&key (to "export-all.cl") (files "./*.lisp"))
  "Export all lisp files' defun/defmacros ..."
  (let ((files (remove-if (^(file) (or (find #\# (file-namestring file))
                                       (equal (pathname-name file) "package")))
                          (directory files))))
    (with-open-file (stream to
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (mapc (curry~ 'code-define-list ~ stream) files))))



(defun package-functions (&optional (package *package*))
  (let ((i 0))
    (do-symbols (sym *package*)
      (and (fbound? sym) (eql *package* (symbol-package sym)) (output "~4d: #'~s ~%" (incf i) sym)))))

(defun package-exported (&optional (package *package*))
  (let ((i 0)) 
    (do-external-symbols (sym package)
      (output "~&~3d: ~A" (incf i) sym))))


(defun all-function-symbols (&optional (package *package*))
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package))
  (the list
       (let ((lst (list))
             (package (find-package package)))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fbound? symb)
                             (eql (symbol-package symb) package))
                    (push symb lst)))
                lst)
               (t
                (error "~S does not designate a package" package))))))



(defmacro mapeach (var list &body body)
  `(mapcar (lambda (,var) ,@body) ,list))


(defmacro hofeach (hof var list &body body)
  "(hofeach #'mapcar x '(1 2 3) (* x x)) => (funcall #'mappcar (lambda (x) (* x x)) '(1 2 3))"
  `(funcall ,hof (lambda (,var) ,@body) ,list))


(defun plist-rows-json (content)
  "Parse the content from (query (:select ...) :plists)"
  (when (and content (list? content) (not (keyword? (first content))))
    (setf content (coerce content 'vector)))
  (com.gigamonkeys.json:json content))


(defparameter *last-time-out* nil)
(defparameter *last-interval* nil)
(defmacro set-time-out (second &body codes)
  "Use (sb-thread:terminate-thread thread) to terminate it"
  `(push (sb-thread:make-thread $((sleep ,second)
                                  ,@codes))
         *last-time-out*))


(defmacro set-interval (second &body codes)
  "Use (sb-thread:terminate-thread thread) to terminate it; `sb-thread:terminate-thread' to stop"
  `(push (sb-thread:make-thread $(loop ,@codes
                                     (sleep ,second)))
         *last-interval*))


(defmacro assure-json-string! (var)
  `(unless (string? ,var)
     (setf ,var (com.gigamonkeys.json:json ,var))))

(defmacro assure-string! (var)
  `(unless (string? ,var)
     (setf ,var (write-to-string ,var))))

(defun json-file (file)
  (com.gigamonkeys.json:parse-json (read-file file)))

(defun clear-last-interval ()
  (let1 (it (first *last-interval*))
    (when (and it (thread-alive-p it))
      (terminate-thread it))))
(defun clear-last-timeout ()
  (let1 (it (first *last-time-out*))
    (when (and it (thread-alive-p it))
      (terminate-thread it))))
;; Backup database every day, delete 5 days before backup??
;; echo "$(date --date="5 day ago" +"%Y-%m-%d").sql"

(defun upload-images-to-server (&key (local-dir "~/Pictures") (remote-dir "/imgs") (server "al"))
  (let* ((local (ppcre:split #\Newline (read-output-string (shell #/find $local-dir -maxdepth 1 \( -iname "*.jpeg" -or -iname "*.png" -or -iname "*.jpg"  -or -iname "*.svg" \) -type f -exec basename {} \;/#))))
         (remote (ppcre:split #\Newline (read-output-string (shell #/ssh root@$server 'find $remote-dir -maxdepth 1 \( -iname "*.jpeg" -or -iname "*.png" -or -iname "*.jpg"  -or -iname "*.svg" \) -type f -exec basename {} \;'/#))))
         (news (set-difference local remote :test #'string=)))          
    (mapc λ(shell #/scp $[local-dir]/$_ $[server]:$[remote-dir]/#) news)
    news))

(defun pull-lisp-projects (&optional (root "~/every-lisp/"))
  (let (errors)
    (dolist (dir (uiop:subdirectories root))
      (setf dir (replace-string (namestring dir) (namestring (probe-file root)) ""))
      (setf dir (replace-string dir "/" ""))
      ($output dir)
      (let1 (err (with-output-to-string (*error-output*)
                   (cmd (fmt "cd ~A~A && git config pull.rebase false && git pull" root dir) *standard-output*)))
        (unless (string= err "")
          (princ err)
          (push (fmt "~%~A~%~A" dir err) errors))))
    (or (string-join errors #\Newline) "OK ✌️ ")))

;; git reset --hard HEAD

