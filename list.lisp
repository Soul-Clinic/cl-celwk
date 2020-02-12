(in-package :celwk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun repeat (n e &aux lst)
    "(repeat 3 ~) => (~ ~ ~)"
    (dotimes (i n lst)
      (push e lst))))

(defun mklist (obj)
  (if (list? obj) obj (list obj))) ;; => (call (if~ 'atom 'list 'identity) obj)

(defun append1 (lst obj) 		;; Unchange lst
  (append lst (list obj)))

(defmacro insert* (obj lst)   	;; Update lst
  "Upldate the LST"
  `(setf ,lst (nconc ,lst (list ,obj))))

(defmacro insert-list (lst lst2)
  (with-gensyms (gst)
    `(let ((,gst ,lst))
       (setf ,gst (nconc ,gst ,lst2)))))

(defun trim-list (lst &key (item "") (test #'equal))
  "Even the ITEM is in the mid of LST, remove it as well(remove empty items)"
  (remove item lst :test test))


(defun suffix-with? (string suffix &key (test 'eql))
  (let ((index (search suffix string :from-end t :test test)))
    (and index
         (= index (- (length string) (length suffix))))))

(defun last* (lst)
  (car (last lst)))

(defun unified-elements? (lst &optional (test 'equal))
  (not (find-if-not #$(call test (aref lst 0) $1) lst)))

(defun same-length? (&rest lists)
  (not (find-if-not #$(= (length (first lists)) (length $1)) lists)))
;; (zero? (call #'length-diff lst1 lst2)))

(defun separate-list (elt-length lst &aux (count-result (ceiling (/ (length lst) elt-length))) result)
  "Each element list's length is ELT-LENGTH as far as possible:
  (separate-list 2 '(1 2 3 4 5)) => ((1 2) (3 4) (5))"
  (dotimes (i count-result (nreverse result))
    (push (subseq lst (* i elt-length)
                  (min (* (1+ i) elt-length)
                       (length lst)))         
          result)))

(defun average-list (count lst)
  "Separate LST to COUNT, as average as possible:
  (average-list 2 '(1 2 3 4 5 6 7)) => '((1 2 3 4) (5 6 7))"
  (separate-list (ceiling (/ (length lst) count)) lst))

(defmacro ifnil! (var value)
  "Set VAR to VALUE if it's nil"
  `(unless ,var
     (setf ,var ,value)))

(defmacro ifnils! (&rest vlst)
  "Set if nil (ifnils! (a v1) (b v2) (c v3)...)
(destructuring-bind (a b &optional c) '(1 2)
     (ifnils! (a 10) (b 30) (c 50))
     (list a b c)) => (1 2 50)"
  `(progn ,@(mapcar #$(destructuring-bind (var val) $1 `(ifnil! ,var ,val))
                    vlst)))

(defun single? (lst)
  (and (cons? lst) (not (cdr lst))))

(defun group-by (fn lst &key (test #'equal) (out #'list))
  (let ((hash (make-hash-table :test test)))
    (mapc #$(let ((key (call fn $1)))
              (unless (gethash key hash)
                (setf (gethash key hash) ()))
              (push $1 (gethash key hash)))
          lst)
    (loop for k being the hash-keys in hash using (hash-value v)
       collect (call out k v))))

(defun flat? (lst)
  (every atom? lst))



;; (flatten '(1 (6 7 2) 3 4 (4 (5 6 ) nil (1 ( 3 (5 6)) 9))))
;; => (1 6 7 2 3 4 4 5 6 1 3 5 6 9)

;; Sun Jan  5 12:10:24 2020
(defun flatten1 (lst &aux $lst)
  (call (self (x)
          (if (list? x)
              (mapcar #'self x)
              (push x $lst)))
        lst)
  (nreverse $lst))

(defun flatten2 (x)
  (call (self (x acc)
          (cond ((null x) acc)
                ((atom x) (cons x acc))
                (t (self (car x) (self (cdr x) acc)))))
        x nil))

(defun flatten3 (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (if (cdr tree) (flatten3 (cdr tree))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun deep-remove-if (test tree)
  "filter+ is more powerful with trees instead of MUST single"
  (let ($lst)
    (dolist (x tree (nreverse $lst))
      (if (atom? x)
          (unless (call test x) (push x $lst))
          (push (deep-remove-if test x) $lst)))))
;; (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (((12))) (13) 22 (20) (9)))
;; => (1 (3 (5)) 7 ((nil)) (13) nil (9))

;; Sun Jan  5 12:10:36 2020
;; (map+ (^(x y) (* y x)) '(10 2 3 (5 9 10) 9 (5 (6 6))) '(1 2 3 (5 9 10) 9 (5 (6 6))))
;; => (10 4 9 (25 81 100) 81 (25 (36 36)))

;; ======================================================================
(defun map+ (fn &rest lsts)
  "Deeply mapcar "
  (apply #'mapcar #%(if (atom? (car *))
                        (apply fn *)
                        (apply #'map+ fn *))
         lsts))
;; Sun Jan  5 18:38:04 2020
;; (filter+ #'> '(1 2 3 (4 11 15) (4) 12 11) '(-1 2 3 (2 1 5) (-4) 12 21))
;; ( 1 (4 11 15) ( 4))
;; (-1 (2  1  5) (-4))
(defun filter+ (test &rest trees)
  "Deep filter, filter the list element in lists as well"
  (labels ((self (fn &rest lsts)
             (let* (($count (length lsts))
                    ($output (repeat $count '())))
               (dotimes (i (length (car lsts)))
                 (let ((items (mapcar #$(nth i $1) lsts)))
                   (if (atom? (car items))
                       (when (apply fn items)
                         (dotimes (n $count)
                           (push (nth n items) (nth n $output))))
                       (setf $output
                             (mapcar (^(a b) (push a b)) (apply #'self fn items) $output))))) ;; push is a macro
               (mapcar #'nreverse $output))))
    (values-list (apply #'self test trees))))

(defun parallel+ (&rest lsts)
  "Deep collect each item from lsts
  (parallel+ '(1 2 (3 4) 5) '(a b (c d) e) '(x x (x x) x))
  => ((1 a x) (2 b x) ((3 c x) (4 d x)) (5 e x))"
  (apply #'mapcar #%(if (atom? (car *))
                        *
                        (apply #'parallel+ *))
         lsts))


(defun remove-duplicate (lst &key (test #'equal)) ;; unique
  (call (reduce~ (^(x next) (adjoin x (call next) :test test))) lst))

(defun find-atom+ (fn tree)
  "Deep find-if for atom"
  (if (atom? tree)
      (if (call fn tree) tree)
      (or (find-atom+ fn (car tree))
          (if* (cdr tree)
               (find-atom+ fn it)))))

;; ======================================================================


(defun 1-to (n &optional (fn 'identity))
  " (n-times 3 #$(+ 2 $1) t) => (3 4 5)  from 1 to n"
  (let ((c (count-args fn)))
    (mapcar #$(apply fn (when (= c 1) (list $1))) (series n))))

(defun 0-below (n)
  (loop for i below n collect i))



(defmacro collect-times ((var count) &body body)
  `(let (it)
     (dotimes (,var ,count it)
       ,@body)))


(defun filled-list (lst filler
                    &optional (placeholder ~)
                    &aux (start 0))
  "(filled-list '(a b c ~ e ~ g) '(~ f)) => (a b c ~ e f g)
 Lacking: (filled-list '(a b c ~ d) '(x x)) => (a b c x d x)"
  (setf lst (copy-list lst))
  ;; (vprint lst) => (a b c ~ d)
  (let ((lacking (- (length filler) (count placeholder lst))))
    (if (plus? lacking)
        (setf lst (nconc lst (repeat lacking placeholder)))))
  ;; (vprint lst) => (a b c ~ d ~)
  (dolist (i filler lst)
    (let ((pos (position placeholder lst :start start)))
      (setf (nth pos lst) i)
      (setf start (1+ pos)))))



(defmacro self-sort (lst fn)
  "(self-sort lst #'>) => (setf lst (sort lst #'>))"
  `(setf ,lst (sort ,lst ,fn)))

(defun previous-matches (fn lst)
  "Separate by the first unmatched element:
 (previous-matches #$(< $1 5) '(1 3 4 5 6 7))  => (values (1 3 4) (5 6 7))"
  (let* ((matched (subseq lst 0 (position-if-not fn lst)))
         (rest (subseq lst (length matched))))
    (values matched rest)))

