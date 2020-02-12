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

(defmacro read-strings (&body outputs)
  `(with-output-to-string (*standard-output*)
     ,@outputs))


(defmacro concats (&rest types)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (^(type)
                 `(defun ,(read-from-string (input "concat-~s" type)) (&rest values)
                   (apply #'concatenate (cons ',type values))))
               types)))
;; (mac (concats string list vector))
(concats string list vector)
(<=> concat-string concat)

(defun empty-string? (string)
  (= (length string) 0))

(defun string-join (string-list &optional (seperator ""))
  "The list must consist of Strings"
  (let ((result (first string-list)))
	(dolist (string (rest string-list) result)
	  (setf result (concat-string result seperator string)))))

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

(defun wrapped-code (code &optional (seperator "=") (count 30))
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
  (flet ((recover (regular)
           (regex-replace-all "\\\\@"
                              (regex-replace-all "(?<!\\\\)@(?=\\d)" regular "\\")
                              "@")))
    (list (if single 'regex-replace 'regex-replace-all) (recover reg) str (recover to))))

(defun @reglax (reg to str &key single)
  (flet ((recover (regular)
           (regex-replace-all "\\\\@"
                              (regex-replace-all "(?<!\\\\)@(?=\\d)" regular "\\")
                              "@")))
    (call (if single #'regex-replace #'regex-replace-all) (recover reg) str (recover to))))

;;(momerize-curry test (@reglax "([a-z0-9])\\1" "[\\1]"))

(defun replace-string (string sub to &optional all)
  (let ((start (search sub string)))
    (if start
        (setf string (concat-string (subseq string 0 start) to (subseq string (+ start (length sub))))))
    (if (and start all)
        (replace-string string sub to all)
        string)))

