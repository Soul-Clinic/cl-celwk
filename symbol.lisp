(in-package :celwk)

(defun symbol-to-key (syb-or-string)
  "(symbol-to-key \"can\") => :can"
  (read-from-format ":~a" syb-or-string))

;; (defun as-keyword (symbol)
;;   (intern (symbol-name symbol) :keyword))

(<=> symbol-to-key as-keyword)
(export 'as-keyword)

(defun key-to-symbol (key)
  ":can => 'can"
  (read-from-string (key-to-string key)))
(defun key=>string (key)
  ":can => \"can\""
  (subseq (write-to-string key) 1))

(defun key-to-string (key)
  "Not for '|Some Data|"
  (subseq (write-to-string key) 1))

(defun symbol-string (symbol)
  "For '|Some Data|"
  (let ((name (write-to-string symbol)))
    (subseq name 1 (1- (length name )))))

(alias-function name-from-1 key-to-symbol)

(defun sharpened-symbol (symbol)
  " xx => #'xx (actually 'xx => '#'xx) For converting to function symbol "
  (read-from-format "#'~s" symbol))
(defun quoted-symbol (symbol)
  " xx => 'xx  (actually 'xx => ''xx) "
  (read-from-format "'~s" symbol))

(defun output-symbol (s)
  "Ensure the symbol of macro is still macro of output code..."
  (if+ symbol? quoted-symbol s))


(defmacro symbol-with-keys (&rest symbols)
  `(list ,@(mapcan λ(list (symbol-to-key _) _) symbols)))
