(in-package :celwk)
(use-package :com.gigamonkeys.json)
(setf com.gigamonkeys.json:*object-type* :hash-table)


(defun hash-keys (hash)
  (loop :for key :being :the :hash-keys :of hash
     :collect key))

(defun hash-values (hash)
  (loop :for value :being :the :hash-values :of hash
     :collect value))

(defmacro sesh (key@hash value)
  "(sesh name@url-client 'good-morning) => (setf (gethash 'name url-client) 'good-morning)"
  (destructuring-bind (key hash)
      (mapcar #'read-from-string (split "@" (write-to-string key@hash)))
    `(setf (gethash ',key ,hash) ,value)))

(defmacro set.hash (hash.key value)
  "(set.hash url.name 'good-morning) => (setf (gethash 'name url) 'good-morning)
  KEY CAN'T BE STRING "
  (destructuring-bind (hash key)
      (mapcar #'read-from-string (split #"\."# (write-to-string hash.key)))
    `(setf (gethash ',key ,hash) ,value)))


"KEYS maybe a list of symbol, string or index, topest first
(gesh (type group 2) hash) => (elt (gethash 'group' (gethash 'type' hash)) 2) "

(defmacro gesh (symbol-keys hash-table)
  "SYMBOL-KEYS => |theAddresses 2 route way| case sensitive, split by space"
  (labels ((parse (keys hash-table)
             (let* ((key (last1 keys))
                    (rest (butlast keys))
                    (next (if rest
                              (parse rest hash-table)	; => Not `(parse ,rest ,hash-table) !! because parse is function, not macor, will excute in run-time only!
                              hash-table)))      
               (cond ((integer? key) `(elt ,next ,key))
                     ((string?  key) `(gethash ,key ,next))
                     ((symbol?  key) `(gethash ,(write-to-string key) ,next))))))

    (let ((keys (mapcar $(or (parse-integer * :junk-allowed t) *)	; => For number index
                        (delete "" (split #" |\""# (string symbol-keys)) :test #'string=))))
      ;; (vprint keys)
      (parse keys hash-table))))

(defmacro gesh-bind (keys hash-table &body code)
  "Use |some_Capitalized_KEY| for case-sensitive key"
  `(destructuring-bind ,keys (list ,@(mapcar (^(key)
                                               `(gesh ,(read-from-format "|~s|" key)
                                                      ,hash-table))
                                             keys))
     ,@code))

(defun symbol-string (symbol)
  (let ((name (write-to-string symbol)))
    (subseq name 1 (1- (length name )))))


(defun camel->lisp (camel-string)
  "aGoodName => a-good-name"
  (regex-replace-all "[A-Z]" camel-string (^(match) (input "-~a" (string-downcase match))) :simple-calls t))

(defun lisp->camel (lisp-string)
  "a-good-name => aGoodName"
  (unless (string? lisp-string)
    (setf lisp-string (write-to-string lisp-string)))
  (regex-replace-all "-([a-z])" lisp-string (^(match alphabet) (input "~a" (string-upcase alphabet))) :simple-calls t))

(defun symbol->camel (symbol-object)
  (lisp->camel (string-downcase (symbol-name symbol-object))))

(defmacro string-key-hash-bind (keys-symbol hash-table &body code)
  "(string-key-hash-bind | nickName gender language city province country avatarUrl | info
       (vprint city nick-name avatar-url country language province gender))"
  (let ((string-keys (split-trim (symbol-string keys-symbol) " ")))
    `(destructuring-bind ,(mapcar λ(read-from-string (camel->lisp _)) string-keys)
         (mapcar λ(gethash _ ,hash-table) ',string-keys)
       ,@code)))

;; (mac (gesh-bind (name type) atable
;;        (list name type)))

(defmethod print-object ((hash hash-table) stream)
  (print-unreadable-object (hash stream :type t)
    (format stream "[ ~{~a~^, ~} ] " (hash-keys hash))))


(defmethod print-object ((hash hash-table) stream)
  (print-unreadable-object (hash stream :type t)
    (maphash (^(k v) (format stream "~&[~a	=> ~a]" k v)) hash)))

(defun print-hash (hash)
  (maphash (^(k v) (output "~%~a: ~a" k v)) hash))



(defun read-until-char (stop &optional stream)
  (let (chars next)
    (until (char= stop (setf next (read-char stream)))
      (push next chars))
    (coerce (nreverse chars) 'string)))

(defun |#[hash-reader]| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((data-string (read-until-char #\] stream)))
    (multiple-value-bind (obj length) (read-from-string data-string)
      `(gesh ,(read-from-format "|~a|" (string-trim '(#\Space) (subseq data-string length))) ,obj))))


(set-dispatch-macro-character #\# #\[ #'|#[hash-reader]|)
;; #[info keyName] => (gethash "keyName" info)
;; #[info someOne 2 firstKiss] => (gethash "firstKiss" (elt (gethash "someOne" info) 2))
;; (setf #[info ...] value) ...

(defmacro dohash ((key-name value-name hash-table) &body body)
  (with-gensyms (next more)
    `(with-hash-table-iterator (,next ,hash-table)
       (loop (multiple-value-bind (,more ,key-name ,value-name)
                 (,next)
               (unless ,more (return nil))
               ,@body)))))

;; (dohash (hero year *h*)
;;   (format t "~A: ~A~%" year hero))

(defmacro with-json-object ((&rest params) data &body codes)
  `(let ,(mapcar λ(list _ (read-from-format "#[~A ~A]" data _)) params)
     ,@codes))
