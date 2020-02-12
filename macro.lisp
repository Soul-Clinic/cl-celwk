(in-package :celwk)

(defmacro with-gensyms ((&rest syms) &body body)
  "(&rest syms) equals to syms
 Just make it more readable, it should be a list"
  `(let ,(loop for n in syms collect `(,n (gensym)))
     ,@body))
;; (defmacro with-gensyms (syms &body body)
;;   `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
;;      ,@body))

(defmacro defconst (name value &optional desc)
  `(if (constant? ',name)	;; boundp
       ,name
       (defconstant ,name ,value ,desc)))
;; (defconst ~ '~)	;; gensym passed to other function will NOT be equal !!

(defmacro unbound-const (cst)
  `(handler-bind ((simple-error 'continue))
     (makunbound ,(output-symbol cst))))
;; Unbound a constant:
;; (defconst xx 100)
;; (handler-bind ((simple-error 'continue))
;;   (makunbound 'xx))
;; or => (unbound-const xx)



(defmacro defsymbol (symbol)
  `(defconst ,symbol ',symbol))
;; (makunbound '~) to cancel constant bound with warning


(defsymbol ~)	;; Declare in this package ONLY ?!
;; (defconst _ ~)




(defmacro for (var start stop &body body)
  "(for n 1 10 (output \"Current: ~a~%\" n)) => INCLUDING 1 and 10"
  (with-gensyms (gstop)
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  "So no need to eval every choice, stop as soon as found. Return: OBJ in CHOICES ?"
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar (^(c) `(eql ,insym ,c))
                     choices)))))

(defmacro until (test &rest body)
  `(do ()
       (,test)
     ,@body))
(defmacro while (test &rest body)
  `(until (not ,test) ,@body))
;; (defmacro while (condition &body body)
;;   `(loop
;;       (unless ,condition (return))
;;       ,@body))


;; Create #'fn & fn at the same time  (setf (symbol-function 'name) lambda)
(defmacro defun+ (fn (&rest args) &body body)
  "Build a function with function symbol and variable with the same name FN:
(FN ...) and (funcall FN ...) are OK"
  `(progn
     (defun ,fn (,@args)
       ,@body)
     (fn+ ,fn)))

(defmacro name-lambda (fname vname)
  `(setf (symbol-function ',fname) ,vname))

(defmacro fn+ (fn)
  "Create a funciton parameter of function FN
  (fn a b c) => (call fn a b c), instead of (call #'fn a b c) "
  `(defparameter ,fn #',fn))

(defmacro defun& (fn)
  "Defin an identical function of the var FN
  (funcall fn a b c) => (fn a b c)"
  `(setf (symbol-function ',fn) ,fn))

(defmacro random-choice (&rest exprs)
  "Similar as above, no need to eval every one, can't do it with function"
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar (^(expr)
                   `(,(incf key) ,expr))
                 exprs))))


(defmacro nil! (var)
  `(setf ,var nil))
(defmacro t! (var)
  `(setf ,var t))
(defmacro opposite! (var)
  "reverse!"
  `(setf ,var (not ,var)))

(alias o! opposite!)

(defmacro self (args &body body)
  "Return a lambda who can call itself with SELF "
  `(labels ((self ,args ,@body)) #'self))

(defmacro alias-macro (new current)	;;TODO: Calculate the the extra args needed
  (let ((args (sb-introspect:function-lambda-list current)))
    `(defmacro ,new ,args 
       (,current ,@args))))


;; (defvar name "Can EriK")
;; name
;; can> (define-symbol-macro ln (length name))
;; ln
;; can> ln
;; 8
;; can> (setf name "Can")
;; "Can"
;; can> ln
;; 3



(defmacro or! (&rest exprs)
  "(mac (or! #'xx+ (prinxc 2) 3 t)) =>
  (or (ignore-errors #'xx+) (ignore-errors (prinxc 2)) (ignore-errors 3)
    (ignore-errors t))"
  `(or ,@(mapcar $(list 'ignore-errors $1) exprs)))

(defmacro try (expr &optional else)
  "(try (/ 10 0) 20) => 20"
  `(or (ignore-errors ,expr) ,else))
