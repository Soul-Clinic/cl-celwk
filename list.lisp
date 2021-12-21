(in-package :celwk)


(<=> position index-of)

(defmacro~ insert-list (~lst lst2)
  `(setf ,~lst (nconc ,~lst ,lst2)))

(defun anyone (lst)
  (elt lst (random (length lst))))

(defun anysome (lst count)
  (let ((total (length lst))
        (temp (copy-list lst))
        indexes x)
    (dotimes (_ (min total count) indexes)
      (setf x (anyone temp))
      (setf temp (delete x temp :count 1))
      (push x indexes))))




(defun trim-list (lst &key (item "") (test #'equal))
  "Even the ITEM is in the mid of LST, remove it as well(remove empty items)"
  (remove item lst :test test))


(defun suffix-with? (string suffix &key (test 'eql))
  (let ((index (search suffix string :from-end t :test test)))
    (and index
         (= index (- (length string) (length suffix))))))

(defun unified-elements? (lst &optional (test 'equal))
  "All items are equal"
  (not (find-if-not $(call test (aref lst 0) $1) lst)))

(defun same-length? (&rest lists)
  (not (find-if-not $(= (length (first lists)) (length $1)) lists)))
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
  `(progn ,@(mapcar $(destructuring-bind (var val) $1 `(ifnil! ,var ,val))
                    vlst)))

(defun single? (lst)
  (and (cons? lst) (not (cdr lst))))

(defun group-by (fn lst &key (test #'equal) (out #'list))
  (let ((hash (make-hash-table :test test)))
    (mapc $(let ((key (call fn $1)))
             (unless (gethash key hash)
               (setf (gethash key hash) ()))
             (push $1 (gethash key hash)))
          lst)
    (loop for k being the hash-keys in hash using (hash-value v)
       collect (call out k v))))

(defun flat? (lst)
  (every atom? lst))



(defun remove-duplicate (lst &key (test #'equal)) ;; unique
  (call (reduce~ (^(x next) (adjoin x (call next) :test test))) lst))

;; ======================================================================


(defmacro define-nth-values (count)
  "Generate first/second/third...COUNTth-value macro"
  `(progn
     ,@(0-below count (^(n)
                        `(defmacro ,(read-from-format "~:r-value" (1+ n)) (value)
                           `(nth-value ,,n ,value)))))) 
(define-nth-values 10)

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
 (previous-matches $(< $1 5) '(1 3 4 5 6 7))  => (values (1 3 4) (5 6 7))"
  (let* ((matched (subseq lst 0 (position-if-not fn lst)))
         (rest (subseq lst (length matched))))
    (values matched rest)))

(defun find-in-depths (depths lst)
  "(find-in-depths '(2 0) '(b c (a d))) => a"
  (reduce (reversed~ 'nth)
          depths
          :initial-value lst))

(defun deep-remove-if (test tree)
  "deep-remove-if is more powerful with trees instead of MUST single"
  (let ($lst)
    (dolist (x tree (nreverse $lst))
      (if (atom? x)
          (unless (call test x) (push x $lst))
          (push (deep-remove-if test x) $lst)))))
;; (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (((12))) (13) 22 (20) (9)))
;; => (1 (3 (5)) 7 ((nil)) (13) nil (9))

;; Sun Jan  5 18:38:04 2020
;; (deep-filter #'> '(1 2 3 (4 11 15) (4) 12 11) '(-1 2 3 (2 1 5) (-4) 12 21))
;; ( 1 (4 11 15) ( 4))
;; (-1 (2  1  5) (-4))


(defun deep-filter (test &rest trees)
  "Deep filter, filter the list element in lists as well"
  (labels ((self (fn &rest lsts)
             (let* (($count (length lsts))
                    ($output (repeat $count '())))
               (dotimes (i (length (car lsts)))
                 (let ((items (mapcar (^(x) (nth i x) lsts))))
                   (if (atom? (car items))
                       (when (apply fn items)
                         (dotimes (n $count)
                           (push (nth n items) (nth n $output))))
                       (setf $output
                             (mapcar (^(a b) (push a b)) (apply #'self fn items) $output))))) ;; push is a macro
               (mapcar #'nreverse $output))))
    (values-list (apply #'self test trees))))

(defun deep-parallel (&rest lsts)
  "Deep collect each item from lsts
  (deep-parallel '(1 2 (3 4) 5) '(a b (c d) e) '(x x (x x) x))
  => ((1 a x) (2 b x) ((3 c x) (4 d x)) (5 e x))
  (pairlis ..) only take 2 lsts and genera (a . b), NOT (a b)"
  (apply #'mapcar %(if (atom? (car *))
                       *
                       (apply #'deep-parallel *))
         lsts))

(defmacro times (code &optional (n 4) (display-result? t))
  "Check the spent times for running CODE N times"
  (when (< n 20)
    (setf n (expt 10 n)))
  `(progn
     (output "~%[ ~r times ]~2%" ,n)
     (time (dotimes (__ (- ,n ,(if display-result? 1 0))) ,code))
     ,(if display-result? code '(values))))



;; (<< 'string-downcase
;;     'to-css-rgb
;;     (mapcar (>> (/ ~ count) 'convert->nearer)	;; <- TODO: Replace ~> to >> ?? Optimize that macro!
;;             (list r+ g+ b+)))

(defun splice (list &key (start 0) (end (length list)) new)
  "Like JavaScript's splice"
  (setf list (cons nil list))                  ;; add dummy cell
  (let ((reroute-start (nthcdr start list)))
    (setf (cdr reroute-start)
          (nconc (make-list (length new))      ;; empty cons cells
                 (nthcdr (- end start)         ;; tail of old list
                         (cdr reroute-start)))
          list (cdr list)))                    ;; remove dummy cell
  (replace list new :start1 start)             ;; fill empty cells
  list)
