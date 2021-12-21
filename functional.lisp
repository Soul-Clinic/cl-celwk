(in-package :celwk)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun count-general-args (args)
    (or (position-if $(find $1 lambda-list-keywords) args)
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


(defun memorize~ (fn &key (test #'equal) timeout)	;; test maybe provided of nil
  "Memorize means in virtue of previous Experience...
 Optimize timeout! (2020/8/7)"
  (let ((cache (make-hash-table :test test)))	;; Closure
    #'(lambda (&rest params)
        (multiple-value-bind (value cached?) (gethash params cache)
          ;; => values-list to save other values!
          (values-list (cond (cached? value)
                             (t (prog1
                                    (setf (gethash params cache)
                                          (multiple-value-list (apply fn params)))	;; Return this
                                  (when timeout
                                    (set-time-out timeout (remhash params cache)))))))))))

(defmacro build-memorized (new fn &key timeout)
  "Memorize function FN to function NEW"
  `(setf (symbol-function ',new) (memorize~ #',fn :timeout ,timeout)))

(defmacro defun~ (name (&rest args) &body code)
  "(defun~ product=>json (pid)
     (postgres-call (:product-info-json pid) :single))"
  (with-gensyms (fname)
    `(progn
       (defun ,fname ,args ,@code)
       (build-memorized ,name ,fname))))



(defun reversed~ (fn)
  %(apply fn (nreverse *)))

(defun reduce~ (fn-x-next &optional init)
  "Build a recursive function 
  (reduce~ (^(x f) (if (f x) x (funcall f))))"
  (self (lst)
    (if lst
        (call fn-x-next (car lst) $(self (cdr lst)))
        (fetch init))))

(defun compose~ (&rest fns)
  (let ((fn1 (last1 fns))
        (other-fns (butlast fns)))
    %(reduce #'call other-fns
             :from-end t
             :initial-value (apply fn1 *))))

(defun pipe~ (&rest fns)
  (apply 'compose~ (nreverse fns)))

(<=> constantly always~)
;; (defun always~ (x) (^(&rest args) (declare (ignore args)) x))	Already has (constantly ...)

(defun is~ (x &key (test #'equal))
  λ(call test x _))

(@defun in~ (&rest sequence &key (test #'equal))
  "(call (in~ 1 2 :test $(= $1 (* $2 10)) 3 4 5) 40) => 4"
  λ(find _ sequence :test test))

(defun bind~ (fn &rest params)
  "(call (bind~ '+ 6 7) 10) "
  %(apply fn (append params *)))

(defun count-args (fn)
  "The REQUIRED arg count"
  (count-general-args (sb-introspect:function-lambda-list fn)))

(<=> count-args required-argcount)

(defun curry~ (fn &rest init)
  "Use ~ for placeholder:
 (call (call (curry~ #'< 1 2 ~ 5 ~) ~ 6) 3) => t
 (call (curry~ #'format t) 'Hello ~a ~a' 'Apple' 'Boy')  "
  (let* ((n (max (length init)
                 (count-args fn)))
         (params (filled-list (repeat n ~) init ~)))
    (if (find ~ params)
        %(apply #'curry~ fn (filled-list params * ~))
        (apply fn params))))

(defun call-compose (&rest fns+arg)
  "Compose all the function except the last, which is the arg of the second last"
  (let ((arg (last1 fns+arg))
        (fns (butlast fns+arg)))
    (call (apply 'compose~ fns) arg)))

(defun call-pipe (arg &rest fns)
  "The first one is arg of the piping functions => curry + pipe "
  (call (apply 'pipe~ fns) arg))


(<=> curry~ *~)
;; (<=> compose~ <~)
;; (<=> pipe~ ~>)

(defun deep-find-atom (fn tree)
  "Deep find-if for atom"
  (if (atom? tree)
      (if (call fn tree) tree)
      (or (deep-find-atom fn (car tree))
          (if* (cdr tree)
               (deep-find-atom fn it)))))
(defun deep-find (item tree)
  (deep-find-atom λ(eql _ item) tree))

(defmacro => (first &rest code-list)
  (unless code-list
    (return-from => first))

  (destructuring-bind (next . others) code-list
    (when (atom? next) 								; A variable  '#'fn or ''fn is not atom! 'fn is
      (setf next (list 'call next ~)))
    (when (find (car next) '(quote function))		; => 'func #'func func are all OK
      (setf next (list (second next) '~)))

    (let* ((safe~ (deep-items-beyond '(=> -> >> << <- ~> <~) ~ next))	;; => -> <- ~> <~ 
           (count (length safe~)))

      (cond ((= count 1) (setf (deep-nth (caar safe~) next) first))
            ((> count 1) (setf next
                               (let ((args (ntimes count #'gensym)))
                                 (dotimes (n count)
                                   (setf (deep-nth (car (nth n safe~)) next) (nth n args)))
                                 `(multiple-value-bind ,args ,first ,next))))
            (t (warn "No placeholder ~~: ~s  ~%" next)))

      `(=> ,next ,@others))))


(alias -> =>)
(defmacro <- (&rest body)
  `(=> ,@(reverse body)))	;; Don't nreverse

;; (alias <~ <-)
(alias >> =>)
(alias << <-)
(defmacro ~> (init &rest code-list &aux (rest (gensym "REST-")) lambda?)
  "Init may be
  Function symbol: (=> #'func ...)
  Function var:    (=> fn1 ...)
  lambda: (=> (list 1 2 _ _) ...)	_ is the arg placeholder 
It will calculate the arg list intelligently...
(call (~> (* _ _) (+ 1 ~) (* 3 ~)) 12 2) => 75"
  (setf init (copy-tree init))

  (cond
    ((and (list? init)		; Function symbol, e.g #'func
          (find (car init) '(quote function)))	
     (let ((required-args (count-args (cadr init))))	; 
       (setf init `(apply ,init ,@(repeat required-args _) ,rest))))
    
    ((atom? init)			; Function lambda var
     (let ((required-args (count-args (symbol-value init))))
       (setf init `(apply ,init ,@(repeat required-args _) ,rest))))
    (t (setf lambda? t)))	;	(+ 10 (call fn1 _))

  (let* ((safe~ (deep-items-beyond '(~> <~) _ init))
         (count (length safe~))
         (args (ntimes count #'gensym)))

    (dotimes (n count)
      (setf (deep-nth (car (nth n safe~)) init) (nth n args)))

    `(lambda (,@args ,@(unless lambda? `(&rest ,rest)))
       (=> ,init ,@code-list))))

(defmacro <~ (&body code-list)
  `(~> ,@(reverse code-list)))



;; '(>> ~> << <~)

;; (alias >> =>)
;; (alias -> =>)

;; (defmacro << (&rest codes)
;;   `(=> ,@(nreverse codes)))

;; (alias <- <<)



(defmacro memorize-curry (fn (currying-fn &rest init))
  `(setf (symbol-function ',fn) (memorize~ (curry~ ',currying-fn ,@init))))

;; (memorize-curry a-z666 regex-replace-all "([a-z0-9])\\1" ~ "[\\1]")
;; (memorize-curry txt
;;     (@reglax "([a-z0-9])@1" "[@1]"))

;; Complement
;; Create a fn's Complement function
;; (!~ fn) => (~fn x y ...) = (not (fn x y ...))
(defmacro !~ (fn)
  "Build a complement function of FN with name ~FN"
  `(defun+ ,(read-from-string (concat "~" (symbol-name fn))) (&rest args)
     (apply (complement #',fn) args)))

(defun and~ (&rest fns)
  "Intersect of functions
(call (and~ (*~ '> ~ 2) (*~ '< ~ 10)) 19) => nil
(call (and~ (*~ '> ~ 2) (*~ '+ ~ 10)) 7)  => 17 "
  %(let (y)
     (dolist (fn fns y)
       (setf y (apply fn *))
       (unless y (return)))))

(defun or~ (&rest fns)
  "Union functions
(call (or~ $(> $1 2) $(+ $1 10)) -19) => -9
(call (or~ $(> $1 2) $(+ $1 10)) 7)   => t"
  %(dolist (fn fns)
     (if* (apply fn *)
          (return it))))

(defun fetch (fn-or-value &rest args)
  "Get the value. Examples: (fetch 123) (fetch #'now)"
  (if (fn? fn-or-value)
      (apply fn-or-value args)
      fn-or-value))

(defun func-name (fn)
  (string-downcase (symbol-name (nth-value 2 (function-lambda-expression fn)))))

