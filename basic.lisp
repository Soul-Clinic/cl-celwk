(in-package :celwk)
(pushnew :celwk *features*)
(defconstant +default-path+ *default-pathname-defaults*)

(format t "~&Welcome 🎊  !!~%")
(setf *print-case* :downcase)

(defmacro alias (new origin &rest init-args)
  `(defmacro ,new (&rest args)
     (append '(,origin) ',init-args args)))


(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))
(defmacro mac+ (expr)
  `(pprint (macroexpand ',expr)))

(defmacro ^ (&body codes)
  `#'(lambda ,@codes))
(defun progns (codes)
  (append '(progn) codes))

(defmacro desc (symbol &optional (type 'function))
  (documentation symbol type))

(defmacro desv (symbol)
  (documentation symbol 'variable))

(defun @doc (symbol text &optional (type 'function))
  "(@doc 'atom? \"Hello atom variable\" 'variable)
   (@doc 'atom? \"Hello atom function\"))"
  (setf (documentation symbol type) text))

(alias output format t)			;; Only print to *standard-output*
(alias input format nil)		;; Return the string


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ends-with? (ends seq &optional (test #'char-equal))
    (unless (listp ends)
      (setf ends (list ends)))
    (find-if (^(end) (equal (search end seq :from-end t :test test)
                            (- (length seq) (length end))))
             ends))
  
  (defun concat-codes (codes)
    (append '(progn) codes)))

(defmacro <=> (origin &rest new-names)
  (concat-codes
   (mapcan (^(new)
             (list `(setf (symbol-function ',new) #',origin)
                   `(defparameter ,new #',new)))
           new-names)))

