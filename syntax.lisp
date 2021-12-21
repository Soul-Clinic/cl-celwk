(in-package :celwk)

(defconst +readtable+ (copy-readtable) "Backup the original readtable")
(defconst +celwk-table+ *readtable* "Syntax style")

(defun restore-syntax ()
  "Restore to the normal syntax, remove $(...) %(...) #/.../# ..."
  (setf *readtable* +readtable+))

(defun celwk-syntax ()
  (setf *readtable* +celwk-table+))

(<=> celwk-syntax cans-syntax)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ntimes (n fn &optional (start 0))
    (loop for i below n
       collect (call fn (+ start i))))

  (defun symbol-int (symb)
    " '%1 => 1   'abc21 => 21 "
    (let* ((str (write-to-string symb))	;; write-to-string is OK
           (from (position-if 'digit-char-p str))
           (to (position-if 'digit-char-p str :from-end t)))
      (and from to (parse-integer (subseq str from (1+ to))))))

  (defun parse-to-lambda (codes &aux (count 0)) ;; #\% => %1 %2  #\$ => 
    "($(+ $1 $2) 10 20) => 30  ($(+ 1 2 *) 10) => 13"

    
    (labels (($n? (symb)
               (scan "^\\$[1-9]\\d*$" (regex-replace "^,*" (write-to-string symb) "")))
             ;; (char= #\$ (find-if-not (^(c) (char= #\, c)) (write-to-string symb))))
             (parse (symb)
               ;; (output "symbol: ~a	atom? ~a ~%" symb (atom symb))
               (if (atom symb)
                   (when ($n? symb)
                     (setf count (max count (or (symbol-int symb) 1))))
                   (mapc #'parse (cdr symb))))
             (var-name (n)
               (read-from-format "$~s" (1+ n))))
      (parse codes)
      
      (let* ((args (ntimes count #'var-name))
             (def "*")
             (star (read-from-string def))) ;; Must use read-from-string instead of '$ for dynamic/context namespace
        
        ;; * and $n can't appear together
        (when (and (not args)
                   (find-if (lambda (v)
                              (let* ((s (write-to-string v))
                                     (n (position (elt def 0) s)))
                                (and n (= (length s) (1+ n)))))
                            (cdrs codes)))
          (setf args (list star)))
        ;; If use ^ instead of lambda, we can't call (%(...) ...), instead must use (call %(...) ...)
        `(lambda ,args ,(if (list? (car codes))
                            `(progn ,@codes)
                            codes)))))

  (defun intern-symbol (char codes)
    (intern (string-upcase (input "~a~s" char codes))))


  (defun dollar-sign (stream char)
    "$(+ 1 $1) => (lambda ($1) (+ 1 $1)
  $abc => |$ABC| (as normal)
  ab$c/abc$ are illegal"
    (declare (ignore char))
    (if (find (peek-char nil stream nil) " )")
        (return-from dollar-sign (intern "$")))
    (let ((codes (read stream nil)))
      (unless codes
        (return-from dollar-sign (intern "$")))
      (if (atom? codes)		;; $02 => $2
          (intern-symbol #\$ codes)
          ;; (intern (string-upcase (input "$~s" codes)));; Can't use read-from-string, or will be infinite recursive
          (parse-to-lambda codes))))

  (set-macro-character #\$ #'dollar-sign t)

  (defun lambda-char (stream char)
    "A lambda with only ONE arg _"
    (declare (ignore char))
    (let ((codes (read stream nil)))
      `(lambda (_) ,codes)))

  (set-macro-character #\λ #'lambda-char t)	; => λ(+ 1 2 _)

  (set-macro-character #\’ (get-macro-character #\'))	;; For Common-Lisp-Recipes











  (defun percent-sign (stream &rest chars)
    ;; (%(list * 4 5 6) 1 2 3)    => ((1 2 3) 4 5 6)
    ;; (apply %(list * 4 5 6) '(1 2 3)) => ((1 2 3) 4 5 6)
    ;; (apply %(append * '(4 5 6)) '(1 2 3)) => (1 2 3 4 5 6)  
    (declare (ignore chars))
    (let ((next-char (peek-char nil stream nil)))
      (if (or (null? next-char) (find next-char " )"))	;; '% 
          (return-from percent-sign (intern "%")))
      ;; (unread-char #\) stream)
      (let ((codes (read stream nil)))
        (unless codes
          (return-from percent-sign (intern "%")))
        (if (atom? codes)
            (intern-symbol #\% codes)
            ;; (intern (string-upcase (input "%~s" codes))) ;; As normal
            ;; If use ^ instead of lambda, we can't call (%(...) ...), instead must use (call %(...) ...)
            `(lambda (&rest *)
               ,(if (list? (car codes))
                    `(progn ,@codes)	;; Multiple codes expressions
                    codes))))))	
  (set-macro-character #\% #'percent-sign t)

  ;; (defmacro with-macro-char ((char function) &body codes)
  ;;   "Update the syntax for CHAR temporarily"
  ;;   `(let ((current (get-macro-character ,char)))
  ;;      (unwind-protect
  ;;           (progn
  ;;             (set-macro-character ,char ,function t)	;; NIL is REQUIRED!
  ;;             ,@codes)
  ;;        (set-macro-character ,char current))))

  ;; (defmacro read-till ((var end &optional stream) &body codes)
  ;;   (with-macro-char (end (get-macro-character #\)))      
  ;;     `(let ((,var (car (read-delimited-list ,end ,stream))))
  ;;        ,@codes)))

  (defun read-until (end &optional stream)
    (do ((s (make-string-output-stream))
         (c (read-char stream) (read-char stream)))
        ((char= c end) (read-from-string (get-output-stream-string s)))
      (write-char c s)))
  
  (defun |#-reader-#| (stream sub-char numarg)
    ;; Useful for regex!   (string-equal #/"text" \1 \2/# "\"text\" \\1 \\2") => t
    (declare (ignore numarg))
    (let (chars src fmt vars)

      (do ((prev (read-char stream) curr)
           (curr (read-char stream) (read-char stream)))
          ((and (char= prev sub-char) (char= curr #\#)))
        (push prev chars))

      (setf src (coerce (nreverse chars) 'string))	;; Get the source

      (with-input-from-string (*standard-input* src)
        (setf fmt
              (with-output-to-string (out)
                (loop
                   (let ((c (read-char *standard-input* nil)))
                     (unless c (return))
                     (case c
                       (#\~ (write-string "~~" out))	; To be compatible with format's constrol-string, cancel it when no  format
                       (#\` (write-char (read-char) out))	; Escape next char
                       ((#\$ #\#)
                        (if (char= (peek-char) #\[)
                            (progn (read-char) ; Jump/Pass #\[
                                   (push (read-until #\])
                                         vars))  
                            (push (read-preserving-whitespace)		; Instead of (read), to RETAIN the WhiteSpace/NewLine !!
                                  vars))
                        (write-string (if (char= c #\$) "~A" "~S") out))                           
                       ((nil))                           
                       (otherwise (write-char c out)))))))
        (if (atom? vars)
            (format nil fmt)
            `(format nil ,fmt ,@(nreverse vars))))))

  ;; (set-dispatch-macro-character #\# #\` #'|#-reader-#|)
  (set-dispatch-macro-character #\# #\" #'|#-reader-#|)
  (set-dispatch-macro-character #\# #\/ #'|#-reader-#|))


(defun @doc (symbol text &optional (type 'function))
  #/(@doc 'atom? "Hello atom variable" 'variable)
  (@doc 'atom? "Hello atom function")/#
  (setf (documentation symbol type) text))


"Fail: Can't return multiple separated elements (like ,@) by set-macro-character
*:default => :default default  Only:  *:default => (:default default)
*:name => :name name
cd ~/Projects && fl ':(\w+) \1'
"



(defmacro delimit (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(defun ddfn (left right fn)
  (set-macro-character right (get-macro-character #\) ))
  (set-dispatch-macro-character #\# left
                                (^(stream char1 char2)
                                  (declare (ignore char1 char2))
                                  (apply fn (read-delimited-list right stream t)))))
;; (delimit #\{ #\} (&rest fns)
;;   "FNS must be function symbol, exclude lambda"
;;   ;; Nope with read-deleimiter-list, must readChar

;;   `(gesh |,(car fns) )|
;; `(pipe~ ,@fns))
;; `(pipe~ ,@(mapcar (^(fn) (sharpened-symbol fn)) fns))
;; #{ fn1 fn2} => (pipe~ fn1 fn2)



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
  "Call then~ only if test~ to calculate the value, else return VAR itself"
  `(call (if~ ',test~ ',then~ 'identity) ,var))

(defmacro if* (test then &optional else)
  "Auto save the TEST result to `it'
  (if* (fn xx)
	   (do it))"
  (with-context (it)
    `(let ((,it ,test))
       (if ,it ,then ,else))))

(defmacro while* (test &body then)
  (with-context (it)
    `(let (,it)
       (while (setf ,it ,test)
         ,@then))))

(defmacro when* (test &body then)
  (with-context (it)
    `(let ((,it ,test))
       (when ,it ,@then))))


;; (defmacro set-if-nil (to value)
;;   `(unless ,to
;;      (setf ,to ,value)))

;; Sun Jan  5 12:10:36 2020
;; (deep-map (^(x y) (* y x)) '(10 2 3 (5 9 10) 9 (5 (6 6))) '(1 2 3 (5 9 10) 9 (5 (6 6))))
;; => (10 4 9 (25 81 100) 81 (25 (36 36)))

;; ======================================================================
(defun deep-map (fn &rest lsts)
  "Deeply mapcar "
  (apply #'mapcar
         %(cond ((atom? (car *)) (apply fn *))
                (t (apply #'deep-map fn *)))
         lsts))


(defmacro let1 (var &body body)
  "Only 1 arg with value, (let1 (i 1) (+ 2 i)) => 3"
  `(let (,var)
     ,@body))
