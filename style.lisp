(in-package :celwk)


(<=> funcall call)
(<=> remove-if-not where filter)
(<=> atom atom?)
(<=> null null?)
(<=> null nil?)

(<=> notany never?)
(<=> some some?)
(<=> some any?)
(<=> every every?)
(<=> notevery notevery?)

(<=> complement !)
(<=> functionp function? fn?)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun end->? (sym &aux (name (write-to-string sym)) (length (length name)))
    (read-from-string (concatenate 'string
                                   (subseq name 0 (- length (if (char= #\- (char name (- length 2))) 2 1)))
                                   "?")))
  (defmacro bool=>? (&rest fnps)
    `(progn ,@(mapcar (^(fnp) `(<=> ,fnp ,(end->? fnp))) fnps))))

(bool=>? oddp evenp numberp zerop plusp minusp
         boundp symbolp fboundp
         integerp floatp complexp arrayp vectorp rationalp realp
         alpha-char-p digit-char-p alphanumericp
         listp consp endp subsetp typep tailp hash-table-p
         constantp keywordp stringp pathnamep pathname-match-p wild-pathname-p packagep
         streamp input-stream-p interactive-stream-p output-stream-p open-stream-p)
#| Equals to:
(<=> numberp number?)
(<=> oddp odd?)
(<=> evenp even?)
|#
