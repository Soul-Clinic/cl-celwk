(in-package :celwk)
(pushnew :celwk *features*)
;; (defconstant +next-line+ "
;; ")

(format t "~&Welcome 😼  !!~%")
(setf *print-case* :downcase)
;; (setf *print-case* :downcase)	;; Care!! cl-base64 comment it first

(defmacro alias (new origin &rest init-args)
  "Alias for macro"
  `(defmacro ,new (&rest args)
     (append '(,origin) ',init-args args))) ;; Equals:  `(,',origin ,,@init-args ,@args)))


(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))
(defmacro mac+ (expr)
  "(macroexpand ..?)"
  `(pprint (macroexpand-1 (macroexpand-1 ',expr))))

(defmacro ^ (&body codes)
  `(lambda ,@codes))
;; ((lambda(x) (+ 2 x)) 6) 	=> 8
;; ((^(x) (+ 2 x)) 6)		=> Error....

(defun progns (codes)
  (append '(progn) codes))

(defmacro desc (symbol &optional (type 'function))
  (documentation symbol type))

(defmacro doc (symbol &optional (type 'function))
  (output "~a" (documentation symbol type)))

(defmacro desv (symbol)
  (documentation symbol 'variable))

(defmacro decc (class)
  "Print the :documentation of DEFCLASS"
  (documentation class 'type))


(alias output format t)			;; Only print to *standard-output*
(alias input format nil)		;; Return the string
(alias fmt format nil)		;; Return the string
(alias man desc)

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))
(defmacro while (test &body body)
  `(until (not ,test) ,@body))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ends-with? (ends seq &optional (test #'char-equal))
    (unless (listp ends)
      (setf ends (list ends)))
    (find-if (^(end) (equal (search end seq :from-end t :test test)
                            (- (length seq) (length end))))
             ends))
  
  (defun concat-codes (codes)
    (append '(progn) codes))

  ;; (eval-when (:compile-toplevel :load-toplevel :execute)
  (defun repeat (n e &aux lst)
    "(repeat 3 ~) => (~ ~ ~)"
    (dotimes (i n lst)
      (push e lst)))
  (defun 1-to (n &optional (fn 'identity))
    " (n-times 3 $(+ 2 $1) t) => (3 4 5)  from 1 to n"
    (let ((c (count-args fn)))
      (mapcar (^(x) (apply fn (when (= c 1) (list x)))) (series n))))

  (defun 0-below (n &optional (fn 'identity))
    (loop for i below n collect (call fn i)))

  (defun mklist (obj)
    (if (list? obj) obj (list obj))) ;; => (call (if~ 'atom 'list 'identity) obj)

  (defun mkint (obj)
    (if (integer? obj) obj (parse-integer obj)))


  (defmacro assure-list (obj)
    `(setf ,obj (mklist ,obj)))
  (defun append1 (lst obj) 		;; Unchange lst
    (append lst (list obj)))

  (defun last1 (lst)
    (car (last lst)))

  (defun (setf last1) (value lst)
    "Update the last one's value"
    (setf (car (last lst)) value))



  (defun insert (val n target)
    "Return a new list that insert VAL to Nth of TARGET"
    (append (subseq target 0 n) (list val) (subseq target n)))

  (defmacro insert* (obj lst)   	;; Update lst
    "Upldate the LST"
    `(setf ,lst (nconc ,lst (list ,obj))))

  (defun print-list (lst)
    (output "~{~A~%~}" lst))

  (defun mapcar-with-index (fn &rest more-lists)
    (apply #'mapcar fn (append more-lists (list (0-below (length (car more-lists)))))))

  (defun deep-list-items (lst &key level redundant? (print? nil))
    "List all atom and its level deeply. 
 (mapcar 'cdr (deep-list-items lst)) equals to flatten"
    (let (result)
      (mapcar-with-index (^(item n &aux (lv (append1 level n)))
                           (if (or (atom? item) redundant?)
                               (setf result (nconc result (list (cons lv item)))))
                           (if (cons? item)
                               (setf result (nconc result (deep-list-items item :level lv :redundant? redundant? :print? nil)))))
                         lst)
      (when print? (print-list result))
      result))

  (defun deep-find-items (item lst)
    "Find all the atom ITEM deeply in LST of ((levels) . ITEM)"
    (filter (^(x) (eql item (cdr x))) (deep-list-items lst)))

  (defun deep-items-beyond (cars item lst)
    "Return the ITEM and its index whose list car is NOT CAR"
    (assure-list cars)
    (remove-if (^(x) (find-if (^(n) (find n cars)) (parent-cars (car x) lst)))  
               (deep-find-items item lst)))

  (defun deep-cars (lst)
    "Return all elements which is FIRST/CAR on its list "
    (filter (^(x) (= 0 (last1 (car x))))
            (deep-list-items lst :print? nil)))

  (defun cdrs (lst)
    "Return all elements which is FIRST/CAR on its list "
    (mapcar #'cdr (filter (^(x) (> (last1 (car x)) 0))
                          (deep-list-items lst :print? nil))))

  (defun deep-find-cars (x lst)
    "Find in deep-cars whose car is X"
    (filter (^(x) (eql x (cdr x))) (deep-cars lst)))

  (defun parent-cars (lvs lst &aux cars)
    "(parent-cars '(1 2 1) '(a (b c (x d e)) f g)) => (a b x) All parent CARs"
    (while (> (length lvs) 0)
      (push (deep-nth (append (butlast lvs) '(0)) lst) cars)
      (setf lvs (butlast lvs)))
    cars)

  (defun deep-nth (ns lst)
    "(deep-nth '(2 1 1) '(1 2 (3 (4 5 6) 7))) => 5"
    (reduce (^(ls n) (and (list? ls) (nth n ls))) ns :initial-value lst))

  (defun (setf deep-nth) (new-value depths lst)
    "Set NEW-VALUE to LST in index of DEPTHS"
    (assure-list depths)
    (let ((ls (deep-nth (butlast depths) lst)))
      (setf (nth (last1 depths) ls) new-value))))

(defmacro <=> (origin &rest new-names)
  (concat-codes
   (mapcan (^(new)
             (list `(setf (symbol-function ',new) #',origin)	;; Or `(setf (fdefinition ',new) #',origin)
                   `(defparameter ,new #',new)))
           new-names)))


(defmacro with-context ((&rest var-names) &body codes)
  `(let ,(mapcar (^(name) `(,name (read-from-string ,(string name))))
          var-names)
     ,@codes))

(defmacro collect-list ((var list) &body body)
  "With variable IT declared"
  (with-context (it)
    `(let (,it)
       (dolist (,var ,list ,it)
         ,@body))))
;; Recursive
(defun flatten (xs)
  "To 1-depth-only list"
  (collect-list (x xs)
    (cond ((null x))
          ((atom x) (setf it (nconc it (list x))))
          (t (setf it (append it (flatten x)))))))

;; (export 'default)
;; (defmacro default (var value)	=> (ifnil! 
;;   "Set a variable if it's nil (Not available for unset defvar without setf)"
;;   `(unless ,var (setf ,var ,value)))

(defmacro define (param &body body)
  "Scheme, for <Structure and Interpretation of Computer Program>"
  (if (listp param)
      `(defun ,(car param) ,(cdr param)
         ,@body)
      `(defparameter ,param ,@body)))
