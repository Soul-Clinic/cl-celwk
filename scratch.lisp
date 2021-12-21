(defmacro => (init &rest codes)	;; Origin
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



(defmacro => (first &rest rest)
  "~ for arg and can be recursive/embed
  Build a lambda if ~ exist in FIRST s-expression, it could have multiple ~ !!"
  (labels ((*=> (init codes)
             (unless codes
               (return-from *=> init))
             (destructuring-bind (next . others) codes
               (when (atom? next) ; a function variable  '#'fn or ''fn is not atom! 'fn is
                 (setf next `(,next ~)))	;; No need to call?
               (when (find (car next) '(quote function))	;; => 'func #'func func are all OK
                 (setf next `(,(second next) ~)))
               (unless (find ~ next)
                 (insert* ~ next))
               ;; Can't use subst, should not deep replace
               (*=> (substitute init ~ next) others))))
    (let ((final (*=> first rest))
          (n 0))
      (when (and (list? final) (deep-find ~ final))
        (let ((func (deep-map $(if (eql $1 ~)
                               (read-from-format "$~d" (incf n))
                               $1)
                          final)))
          (setf final (read-from-string (input "$~s" func)))))
      final)))



(defparameter test '(>> 'string-downcase
                     'to-css-rgb
                     (mapcar (>> $(/ ~ count) 'convert->nearer)	;; <- TODO: Replace ~> to >> ?? Optimize that macro!
                      (list r+ g+ b+))))






















(defun lastguy (x) (car (last x))) =>  LASTGUY
(define-setf-expander lastguy (x &environment env)
  "Set the last element in a list to the given value."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (rplaca (last ,getter) ,store) ,store)
              `(lastguy ,getter))))) =>  LASTGUY
(setq a (list 'a 'b 'c 'd)
      b (list 'x)
      c (list 1 2 3 (list 4 5 6))) =>  (1 2 3 (4 5 6))
(setf (lastguy a) 3) =>  3
(setf (lastguy b) 7) =>  7
(setf (lastguy (lastguy c)) 'lastguy-symbol) =>  LASTGUY-SYMBOL
a =>  (A B C 3)
b =>  (7)
c =>  (1 2 3 (4 5 LASTGUY-SYMBOL))








;;; Setf expander for the form (LDB bytespec int).
;;; Recall that the int form must itself be suitable for SETF.
(define-setf-expander ldb (bytespec int &environment env)
  (multiple-value-bind (dummies
                        vals
                        stores
                        store-form
                        access-form)
      (get-setf-expansion int env);Get setf expansion for int.
    (let ((btemp (gensym))     ;Temp var for byte specifier.
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores))) ;Temp var of newValue for int to store.
      (if (cdr stores) (error "Can't expand this."))
;;; Return the setf expansion for LDB as five values.
      (values (cons btemp dummies)       ;Temporary variables.
              (cons bytespec vals)     ;Value forms.
              (list store)             ;Store variables. (setf X b): store => X
              `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                 ,store-form
                 ,store)               ;Storing form.
              `(ldb ,btemp ,access-form) ;Accessing form.
              ))))

;; (defmacro => (first &rest code-list)
;;   (unless code-list
;;     (return-from => first))
;;   (destructuring-bind (next . others) code-list
;;     (when (atom? next) 								; A variable  '#'fn or ''fn is not atom! 'fn is
;;       ;; (setf next `(call ,next ~)))
;;       (setf next (list 'call next ~)))
;;   (when (find (car next) '(quote function))		; => 'func #'func func are all OK
;;     (vprint (list (second next) '~) `(,(second next) ~))
;;     (setf next (list (second next) '~)))
;;   ;; (setf next `(,(second next) ,~)))
;;   ;; (vp next others ~))
;;   ;; (vprint first next)
;;   (let* ((safe~ (deep-items-beyond '(=> -> <- ~> <~) ~ next))	;; => -> <- ~> <~ 
;;          (count (length safe~)))
;;     ;; (vp count safe~ next)
;;     (cond ((= count 1) (setf (deep-nth (caar safe~) next) first))
;;           ((> count 1) (setf next
;;                              (let ((args (ntimes count #'gensym)))
;;                                (dotimes (n count)
;;                                  (setf (deep-nth (car (nth n safe~)) next) (nth n args)))
;;                                `(multiple-value-bind ,args ,first ,next))))
;;           (t (warn "No safe~~ of argument placeholder ~%")))
;;     `(=> ,next ,@others))))



;; (defmacro => (&rest code-list)
;;   "~ for arg and can be recursive/embed
;;   Build a lambda if ~ exist in FIRST s-expression, it could have multiple ~ !!
;;   Fail when embedded =>"
;;   ;; '(:todo multiple-value-bind for multiple values)
;;   ;; (=> (values (fn1) (fn2))
;;   ;;     (fn3 ~ x ~))  ; => (fn3 (fn1) x (fn2))   ~ must be outside inner (=> ...)
  
;;   (labels ((*=> (code-list)	;; USE LABELS!! NOT MACROLET !!!
;;              (vprint code-list)
;;              (destructuring-bind (init &optional next . others) code-list
;;                (unless next
;;                  (vprint init)               
;;                  (return-from *=> init))
;;                (when (> (count ~ (flatten next)) 1)		;; ; TODO: Embedded => ... (values ...) + Recursive =>
;;                    (warn "MORE THAN ONE ~~ in ~s, it may run unexperted." next))
;;                  ;; next find =>
;;                (*=> (cons (subst init ~ next) others)))))   			; Or substitue to in case of  deep replace??

;;     (vprint code-list)
;;     (setf code-list
;;           (mapcar (^(code n)
;;                     (when (atom? code)
;;                       (setf code `(call ,code ~)))
;;                     (when (find (car code) '(quote function))		; => 'func #'func func are all OK
;;                       (setf code `(,(second code) ~)))

;;                     (dolist (item (where $(eql (cdr *) '=>)
;;                                          (deep-cars code t)))
;;                       (vprint item)
;;                       (let ((nths (butlast (car item))))
;;                         (setf (deep-nth nths code)
;;                               (*=> (cdr (deep-nth nths code))))))
                    
;;                     (unless (or (= n 0) (deep-find ~ code))
;;                       (insert* ~ code))
;;                     (vprint code)
;;                     code)
;;                   code-list
;;                   (0-below (length code-list))))
;;     (vprint code-list)
    
;;     (let ((expression (*=> code-list)))
;;       (when (and (list? expression)
;;                  (not (deep-find-atom $(find * '(>> => << <-)) expression))	  
;;                  (deep-find ~ expression))
;;         (output "Before: ~a~%" expression)
;;         (let* ((n 0)
;;                (func (deep-map $(if (eql $1 ~)
;;                                     (read-from-string (input "$~d" (incf n)))
;;                                     $1)
;;                                expression)))
;;           (setf expression (read-from-string (input "$~s" func)))))
;;       ;; (vprint expression)
;;       expression)))

