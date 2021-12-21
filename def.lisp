(in-package :celwk)
(defun parallel (&rest lists)
  (apply #'mapcar #'list lists))

(defmacro with-gensyms ((&rest syms) &body body)
  "(&rest syms) equals to syms
 Just make it more readable, it should be a list
 Keep this style: (with gensyms (~x ~y) ..."
  `(let ,(loop for n in syms collect `(,n (gensym ,(concatenate 'string (string n) "-"))))
     ,@body))

(defmacro once-only ((&rest vars) &body body)
    (let ((args (mapcar $(gensym (string *)) vars)))
      `(with-gensyms (,@args)
         `(let (,@(parallel (list ,@args) (list ,@vars)))
            ,@(let (,@(parallel vars args))
                (list ,@body))))))

#|
(defmacro once-only ((&rest vars) &body body)
  "April.15th.2020 "
  (let ((args (mapcar (~> #'string #'gensym) vars)))
    `(with-gensyms (,@args)
       `(let (,@(parallel (list ,@args) (list ,@vars)))
          ,@(let (,@(parallel vars args))
              (list ,@body))))))

From the book [http://pcl.celwk.com/08.macros-defining-your-own.htm] :

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro once-only ((&rest vars) &body body)
  (let ((gensyms (loop for n in vars collect (gensym))))
    `(with-gensyms (,@gensyms)
       `(let (,,@(loop for g in gensyms
                    for n in vars
                    collect ``(,,g ,,n)))
          ,(let (,@(loop for n in vars
                      for g in gensyms
                      collect `(,n ,g)))
             ,@body)))))
|#

;; Create #'fn & fn at the same time  (setf (symbol-function 'name) lambda)
(defmacro defun+ (fn (&rest args) &body body)
  "Build a function with function symbol and variable with the same name FN:
(FN ...) and (funcall FN ...) are OK"
  `(progn
     (defun ,fn (,@args)
       ,@body)
     (fn+ ,fn)))

(defmacro defun& (fn)
  "Defin an identical function of the var FN
  (funcall fn a b c) => (fn a b c)"
  `(setf (symbol-function ',fn) ,fn))

;; (alias-function name-from-1 key-to-symbol)
(defmacro defmacro~ (name (&rest args) &body body)
  "When arg start with ~, then make sure it execute only once!
  (defmacro~ in (~obj &rest choices)
	`(or ,@(mapcar (^(c) `(eql ,~obj ,c)) choices)))"
  (flet ((^~? (sym)
           (char= #\~ (elt (write-to-string sym) 0))))

    (let* ((~vars (filter #'^~? (flatten args)))
           (vars (deep-map (if~ #'^~? 'name-from-1 'identity) args))
           (lets (mapcar (^(~v) `(list ,~v ,(name-from-1 ~v))) ~vars)))  ;;(`(,var~ ,var) `(,var2~ ,var2)) ~> ((list var~ var) (list var2~ var2))
      
      `(defmacro ,name (,@vars)
         ,(let ((comment (first body)))
            (when (and (string? comment)
                       (> (length body) 1)) ; comment
              (setf body (cdr body))
              comment))
         (with-gensyms (,@~vars)
           `(let (,,@lets) 
              ,,@body))))))



(defmacro~ in (~obj &rest choices)
  "So no need to eval every choice, stop as soon as found. Return: ~OBJ in CHOICES ?!
  (in 3 2 3 (print 4)) => Won't print 4"
  `(or ,@(mapcar (^(c) `(eql ,~obj ,c)) choices)))

;; (defmacro in (obj &rest choices)
;;   "So no need to eval every choice, stop as soon as found. Return: OBJ in CHOICES ?"
;;   (with-gensyms (insym)
;;     `(let ((,insym ,obj))
;;        (or ,@(mapcar (^(c) `(eql ,insym ,c))
;;                      choices)))))

