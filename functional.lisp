(in-package :celwk)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun count-general-args (args)
    (or (position-if #$(find $1 lambda-list-keywords) args)
        (length args)))

  (defun general-args (args)
    "(general-args '(xx p1 p2 &rest rest &key k1 k2)) =>(xx p1 p2)"
    (subseq args 0 (count-general-args args)))

  (defun getf* (lst key &key (test #'equal) from-end)
    "Like GETF, but also get when the KEY is in even? number of LST"
    (if* (position key lst :test test :from-end from-end)
         (nth (1+ it) lst)))
  ;; (getf '(xx p1 p2 &rest rest &key k1 k2) '&rest)  => nil
  ;; (getf* '(xx p1 p2 &rest rest &key k1 k2) '&rest) => rest

  (defun arg-list (alist)
    "(arg-list '(a b &rest r &key k1 k2)) => (:general '(a b) :rest 'r :key '(:k1 :k2))"
    (let (total)
      (setf (getf total :general) (general-args alist)
            (getf total :rest) (getf* alist '&rest)
            (getf total :key) (rest (member '&key alist)))
      total))
  (defun general-&-keys (&rest rest)
    "(general-&-keys 21 34 'yo :oo 5 :xx 99 88)
 => (21 34 yo 88)	;; Including rest
	(:oo 5 :xx 99)"
    (do ((i 0) lst keys)
        ((>= i (length rest))
         (values (nreverse lst) keys))
      (let ((value (elt rest i)))
        (if (keywordp value)
            (progn
              (setf keys (nconc keys (subseq rest i (+ i 2)))) ;; => (:k1 xx :k2 cc)
              (incf i 2))
            (and (push value lst)
                 (incf i))))))

  ;; (defun@ .. &rest ... &key ...)
  (defmacro @defun (fn params &body code)
    "Define a function whose rest & key don't mixture(exclusive). No optional"
    (destructuring-bind (&key general rest key) (arg-list params)
      `(defun ,fn (,@general &rest @rest-&-keys@)	;; Use gensym if don't want visible
         (destructuring-bind (,rest &key ,@key)
             (multiple-value-bind (rest keys)
                 (apply #'general-&-keys @rest-&-keys@)
               (nconc (list rest) keys))
           ,@code)))))
;; (@defun test-fn (a b c &rest o &key k1 k2)
;;   (list a b c :rest o :k1 k1 :k2 k2))
;; (test-fn 1 3 5 8 9 :k1 'kk1 :k2 'kk2 999)
;; => (1 3 5 :rest (8 9 999) :k1 kk1 :k2 kk2)


(defun memorize~ (fn &optional test)	;; test maybe provided of nil
  "Memorize means in virtue of previous Experience..."
  (let ((cache (make-hash-table :test (or test #'equal))))	;; Closure
    #%(multiple-value-bind (value cached?) (gethash * cache)
        (values-list (if cached?
                         value ;;(and (princ "Cached") val)
                         (setf (gethash * cache)
                               (multiple-value-list (apply fn *))))))))

(defmacro build-memorized (new fn &optional (test 'equal))
  "Memorize function FN to function NEW"
  `(setf (symbol-function ',new) (memorize~ #',fn #',test)))

(defun reversed~ (fn)
  #%(apply fn (nreverse *)))

(defun reduce~ (fn-x-next &optional init)
  "Build a recursive function 
  (reduce~ (^(x f) (if (f x) x (funcall f))))"
  (self (lst)
    (if lst
        (call fn-x-next (car lst) #$(self (cdr lst)))
        (fetch init))))

(defun compose~ (&rest fns)
  (let ((fn1 (last* fns))
        (other-fns (butlast fns)))
    #%(reduce #'call other-fns
              :from-end t
              :initial-value (apply fn1 *))))

(defun pipe~ (&rest fns)
  (apply 'compose~ (nreverse fns)))

(defun always~ (x) (^(&rest args) (declare (ignore args)) x))

(defun is~ (x &key (test #'equal))
  #$(call test x $1))

(@defun in~ (&rest sequence &key (test #'equal))
  "(call (in~ 1 2 :test #$(= $ (* $2 10)) 3 4 5) 40) => 4"
  #$(find $1 sequence :test test))

(defun bind~ (fn &rest params)
  "(call (bind~ '+ 6 7) 10) "
  #%(apply fn (append params *)))

(defun count-args (fn)
  (count-general-args (sb-introspect:function-lambda-list fn)))

(defun curry~ (fn &rest init)
  "Use ~ for placeholder:
 (call (call (curry #'< 1 2 ~ 5 ~) ~ 6) 3) => t
 (call (curry~ #'format t) 'Hello ~a ~a' 'Apple' 'Boy')  "
  (let* ((n (max (length init)
                 (count-args fn)))
         (params (filled-list (repeat n ~) init ~)))
    (if (find ~ params)
        #%(apply #'curry~ fn (filled-list params * ~))
        (apply fn params))))

(defun call-compose (&rest fns+arg)
  "Compose all the function except the last, which is the arg of the second last"
  (let ((arg (last* fns+arg))
        (fns (butlast fns+arg)))
    (call (apply '<~ fns) arg)))

(defun call-pipe (arg &rest fns)
  "The first one is arg of the piping functions => curry + pipe "
  (call (apply '~> fns) arg))


(<=> curry~ *~)
(<=> compose~ <~)
(<=> pipe~ ~>)
;; (<=> call-compose <<)
;; (<=> call-pipe >>)

(defmacro => (init &rest codes)
  "~ for arg and can be recursive/embed"
  (unless codes
    (return-from => init))
  (destructuring-bind (next . others) codes
    (when (atom? next) ; a function variable
      (setf next `(call ,next ~)))
    (when (find (car next) '(quote function))	;; => 'func #'func func are all OK
      (setf next `(,(second next) ~)))
    (unless (find ~ next)
      (insert* ~ next))
    `(=> ,(substitute init ~ next) ,@others)))    ;; Can't use subst, should not deep replace
(alias >> =>)

(defmacro << (&rest codes)
  `(=> ,@(nreverse codes)))

;; (defmacro *~~ (fn &rest init)
;;   "Like curry~, but the first arg of fn can be a symbol without #\'.
;; So FN must be name of function, can't be a function variable name "
;;   `(*~ ,(output-symbol fn) ,@init))


(defmacro memorize-curry (fn (currying-fn &rest init))
  `(setf (symbol-function ',fn) (memorize~ (curry~ ',currying-fn ,@init))))

;; (memorize-curry a-z666 regex-replace-all "([a-z0-9])\\1" ~ "[\\1]")
;; (memorize-curry txt
;;     (@reglax "([a-z0-9])@1" "[@1]"))

;; Complement
;; Create a fn's Complement function
;; (~ fn) => (~fn x y ...) = (not (fn x y ...))
(defmacro ~ (fn)
  "Build a complement function of FN with name ~FN"
  `(defun+ ,(read-from-string (concat "~" (symbol-name fn))) (&rest args)
     (apply (complement #',fn) args)))


(defun and~ (&rest fns)
  "Intersect of functions
(call (and~ (*~ '> ~ 2) (*~ '< ~ 10)) 19) => nil
(call (and~ (*~ '> ~ 2) (*~ '+ ~ 10)) 7)  => 17 "
  #%(let (y)
      (dolist (fn fns y)
        (setf y (apply fn *))
        (unless y (return)))))

(defun or~ (&rest fns)
  "Union functions
(call (or~ #$(> $1 2) #$(+ $1 10)) -19) => -9
(call (or~ #$(> $1 2) #$(+ $1 10)) 7)   => t"
  #%(dolist (fn fns)
      (if* (apply fn *)
           (return it))))

(defun fetch (fn-or-value &rest args)
  "Get the value. Examples: (fetch 123) (fetch #'now)"
  (if (fn? fn-or-value)
      (apply fn-or-value args)
      fn-or-value))

(defun function-name (fn)
  (string-downcase (symbol-name (nth-value 2 (function-lambda-expression fn)))))

