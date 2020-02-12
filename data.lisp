(in-package :celwk)
(defmacro sesh (k v h)
  "set hash"
  `(setf (gethash ,k ,h) ,v))
(alias set-hash sesh)
(alias sethash sesh)

(defun lish (hash &optional (fn #'cons))
  "list hash"
  (let (lst)
    (maphash (^(k v) (push (call fn k v) lst)) hash)
    (nreverse lst)))
(<=> lish lash list-hash)
