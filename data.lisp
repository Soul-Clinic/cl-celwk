(in-package :celwk)
;; (defmacro sesh (k v h)
;;   "set hash"
;;   `(setf (gethash ,k ,h) ,v))
;; (alias set-hash sesh)
;; (alias sethash sesh)

(defun lish (hash &optional (fn #'cons))
  "list hash"
  (let (lst)
    (maphash (^(k v) (push (call fn k v) lst)) hash)
    (nreverse lst)))
(<=> lish lash list-hash)


(defun read-from-format (control-string &rest format-args)
  (read-from-string (apply #'format nil control-string format-args)))

(defmacro multiple-json-bind ((&rest keys) json-string &body code)
  `(let ,(mapcar λ(list _ (read-from-format "#[~a ~a]" json-string _)) keys)
     ,@code))

