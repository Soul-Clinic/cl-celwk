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



(defmacro bound-type (name)
  "(values :value :function)"
  `(cond
     ((boundp  ',name) (funcall #'values :value (when (fboundp ',name) :function)))
     ((fboundp ',name) (values :function nil))
     (t (values nil nil))))


(defmacro vprint (&rest vars)
  "(vprint a b x):
 a: **
 b: **
 x: **"
  `(output "~@{~a: ~s~%~}"
           ,@(mapcan (^(x) `(',x ,x)) vars)))

(defmacro second-value (code)
  `(nth-value 1 ,code))

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






;; (set-macro-character #\= (get-macro-character #\;)) => for copy the example code from LispWorkds
;; (set-macro-character #\= nil)	=> Cancel when your code includes '='
