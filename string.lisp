(in-package :celwk)
#| http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_stg.htm
MutableStrings:
(with-output-to-string (*standard-output*)
(write-string "books" nil :end 3)
(write-string "worms" *standard-output* :start 1))
=> "boo" + "orms" = "booorms"

write-string /write-line write-char

(with-output-to-string (*standard-output*)
(output "What"))
== (input "What")
|#

;; (=> (cmd "node -e 'console.log(JSON.stringify(process.env))'")
;;     (string-trim '(#\Newline) ~)
;;     (parse-json ~))

(defun trim (string)
  (string-trim '(#\Newline #\Space #\Tab #\Return) string))

(defmacro read-output-string (&body outputs)
  `(trim (with-output-to-string (*standard-output*)
           ,@outputs)))

(defun split-trim (string &optional (spliter "/"))
  (delete "" (split spliter string) :test #'string=))

(defun to-simple-string (string)
  (input "~a" string))

(defmacro insure-string (string)
  `(unless (string? ,string)
     (setf ,string (write-to-string ,string))))


(defmacro concats (&rest types)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (^(type)
                 `(defun ,(read-from-format "concat-~s" type) (&rest values)
                    (apply #'concatenate (cons ',type values))))
               types)))
;; (mac (concats string list vector))
(concats string list vector)
(<=> concat-string concat)
(<=> concat-string string+)

(defun empty-string? (string)
  (= (length string) 0))

(defun string-or-nil (value)
  (when (string? value) value))



(defun string-join (string-list &optional (separator ""))
  "The list must consist of Strings, return NIL if string-list is NIL"
  (unless string-list
    (return-from string-join nil))
  (with-output-to-string (*standard-output*)
    (loop (princ (pop string-list))
          (unless string-list (return))
          (princ separator))))
;; (let ((result (first string-list)))
;;   (dolist (string (rest string-list) result)
;;     (setf result (concat-string result separator string)))))

(defun string-repeat (string n)
  "Create a string repeated n times"
  (string-join (fill (make-list n) string)))

(defun string-of-code (code)
  "Return lowercase string from the code
  (read-from-string ...) useful as well"
  (string-downcase (write-to-string code)))
;; write-to-string 		symbol => string
;; read-from-string  	string => symbol
;; (eval (read-from-string "(+ 1 2 3)"))  => 6
;; (equal '(+ 1 2 3) (read-from-string "(+ 1 2 3)"))  t !!!

(defmethod aesthetic (sth)
  (input "~a" sth))



(defun wrapped-code (code &optional (seperator "=") (count 20))
  (input "~A~%~A~%~2:*~A"
         (string-repeat seperator count)
         (string-of-code code)))

(defun join (string-list
             &key (seperator " ") (out nil) (fn #'identity)              
             &aux (seperator (regex-replace-all "~" seperator "~~")))
  "Join the string list"           
  (format out
          (input "~~{~~A~~^~A~~}" seperator) (mapcar fn string-list)))

;; (@replace-all "([a-z])@1" "~@1~" "I need yooooo..")
;; (defmacro @replace-all (reg to str)
;; (flet ((recover (regular)
;;          (let ((rreg (regex-replace-all "(?<!\\\\)@(?=\\d)" regular "\\")))
;;            (regex-replace-all "\\\\@" rreg "@"))))
;;   `(regex-replace-all ,(recover reg) ,str ,(recover to))))

(defmacro @replex (reg to str &key single)
  "@1 @2... match (...) "
  (flet ((recover (regular)
           (regex-replace-all "\\\\@"
                              (regex-replace-all "(?<!\\\\)@(?=\\d)" regular "\\")
                              "@")))
    (list (if single 'regex-replace 'regex-replace-all) (recover reg) str (recover to))))

(defun @reglax (reg to str &key single)
  "@1 @2... match (...) regex-replace-all "
  (flet ((recover (regular)
           (regex-replace-all "\\\\@"
                              (regex-replace-all "(?<!\\\\)@(?=\\d)" regular "\\")
                              "@")))
    (call (if single #'regex-replace #'regex-replace-all) (recover reg) str (recover to))))

;;(momerize-curry test (@reglax "([a-z0-9])\\1" "[\\1]"))

(defun replace-string (string sub to &key (all t) (from 0))
  (let ((start (search sub string :start2 from)))
    (if start
        (setf string (concat-string (subseq string 0 start) to (subseq string (+ start (length sub))))))
    (if (and start all)
        (replace-string string sub to :all t :from (+ start (length to))) ;; In case of TO contains SUB
        string)))

