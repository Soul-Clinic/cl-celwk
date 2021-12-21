(in-package :celwk)

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
;; (defsymbol _)
(defparameter _ '_)


(defmacro name-lambda (fname vname)
  `(setf (symbol-function ',fname) ,vname))

(defmacro alias-function (new old)
  `(setf (symbol-function ',new) #',old))

(defmacro fn+ (fn)
  "Create a funciton parameter of function FN
  (fn a b c) => (call fn a b c), instead of (call #'fn a b c) "
  `(defparameter ,fn #',fn))


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
