(in-package :celwk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ntimes (n fn &optional (start 0))
    (loop for i below n
       collect (call fn (+ start i))))

  (defmacro collect-list ((var list) &body body)
    `(let (it)
       (dolist (,var ,list it)
         ,@body)))
  ;; Recursive
  (defun flatten (xs)
    "To 1-depth-only list"
    (collect-list (x xs)
      (cond ((null x))
            ((atom x) (setf it (nconc it (list x))))
            (t (setf it (append it (flatten x)))))))

  (defun symbol-int (symb)
    " '%1 => 1   'abc21 => 21 "
    (let* ((str (string symb))	;; write-to-string is OK
           (from (position-if 'digit-char-p str))
           (to (position-if 'digit-char-p str :from-end t)))
      (and from to (parse-integer (subseq str from (1+ to))))))

  (defun parse-to-lambda (code &optional (separator #\$) &aux (count 0)) ;; #\% => %1 %2  #\$ => 
    "(funcall $(+ $1 $2) 10 20) => 30
	$0 => Count of args $ => $1
  (call $(+ 1 2 $ $0 $2) 0 2) => 7:  $=$1 => 0"
    (labels ((parse (symb)
               (if (atom symb)
                   (when (char= separator (char (write-to-string symb) 0))
                     (setf count (max count (or (symbol-int symb) 1))))
                   (mapc #'parse symb)))
             (var-name (n)
               (read-from-string (input "~c~s" separator (1+ n)))))
      (parse code)
      
      (let ((args (ntimes count #'var-name))
            ($ (read-from-string "$")) ;; Must use read-from-string instead of '$ for dynamic/context namespace
            (acount (read-from-string "$0")))
        ;; $ and $1 can't appear at the together
        (when (find $ (flatten code))
          (setf (car args) $))
        (when (find acount (flatten code))
          (setf args (append args '(&aux) `((,acount ,count)))))
        `(^,args ,code))))
  
  (defun dollar-sign (stream char)
    "$(+ 1 $1) => (^($1) (+ 1 $1)
  $abc => |$ABC| (as normal)
  ab$c/abc$ are illegal"
    (declare (ignore char))
    (if (find (peek-char t stream nil) ")")
        (return-from dollar-sign (intern "$")))
    (let ((code (read stream nil)))
      (unless code
        (return-from dollar-sign (intern "$")))
      (if (atom? code)
          (intern (string-upcase (input "$~s" code)));; Can't use read-from-string, or will be infinite recursive
          (parse-to-lambda code))))

  (set-macro-character #\$ #'dollar-sign t)
  ;; (set-dispatch-macro-character #\# #\$ #'dollar-sign)
  
  (defun percent-sign (stream &rest chars)
    ;; (call  %(list * 4 5 6) 1 2 3)    => ((1 2 3) 4 5 6)
    ;; (apply %(list * 4 5 6) '(1 2 3)) => ((1 2 3) 4 5 6)
    ;; (apply %(append * '(4 5 6)) '(1 2 3)) => (1 2 3 4 5 6)  
    (declare (ignore chars))
    (let ((code (read stream)))
      (if (atom? code)
          (intern (string-upcase (input "%~s" code))) ;; As normal
          `(^(&rest *) ,code))))
  (set-macro-character #\% #'percent-sign t))
;; (set-dispatch-macro-character #\# #\% #'percent-sign))
;; Global exported! No need to be added in package export


"Fail: Can't return multiple separated elements (like ,@) by set-macro-character
*:default => :default default  Only:  *:default => (:default default)
*:name => :name name
cd ~/Develop/Lisp/mine && fl ':(\w+) \1'
"

(defmacro delimit (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(defun ddfn (left right fn)
  (set-macro-character right (get-macro-character #\) ))
  (set-dispatch-macro-character #\# left
                                (^(stream char1 char2)
                                  (declare (ignore char1 char2))
                                  (apply fn (read-delimited-list right stream t)))))
(delimit #\{ #\} (&rest fns)
  "FNS must be function symbol, exclude lambda"
  `(pipe~ ,@fns))
;; `(pipe~ ,@(mapcar (^(fn) (sharpened-symbol fn)) fns))
;; #{fn1 fn2} => (pipe~ fn1 fn2)

(defun $cancel-macor-char (char)
  (set-macro-character char nil))

(defun $cancel-dispatch-macor-char (char1 char2)
  "#\# #\[ or  #\# #\{   =>?  #\# #\] necessary?"
  (set-dispatch-macro-character char1 char2 nil))


(defun series (to &key (from 1))
  "(series 4) => (1 2 3 4)"
  (loop :for i :from from :to to
     collect i))

;; (delimit #\[ #\] (from to)
;;   "Including FROM & TO: #[1 4] => (1 2 3 4)"
;;   (if (find-if-not $(number? $1) (list from to))
;;       `(series ,to :from ,from)
;;       `(quote ,(series to :from from))))	; => `'(,@(series (1+ to) :from from))))





(defun if~ (if then &optional else)
  "(call (if~ 'odd? #'print $(+ 10 $1)) 123) "
  %(if (apply if *)
       (apply then *)
       (if else (apply else *))))

(defmacro if+ (test~ then~ var)
  "Call then~ only if test~ to calculate the value"
  `(call (if~ ',test~ ',then~ 'identity) ,var))

(defmacro if* (test then &optional else)
  "Auto save the TEST result to `it'"
  ;; Otherwise it will be celwk::it !!
  ;; (read-from-string...) depeneds on evaling context
  (let ((it (read-from-string "it")))
    `(let ((,it ,test))
       (if ,it ,then ,else))))
;; (if* (gethash xx yy)
;;      (output it))

