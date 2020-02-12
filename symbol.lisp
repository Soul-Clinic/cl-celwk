(in-package :celwk)

(defun symbol-to-key (syb-or-string)
  "(symbol-to-key \"can\") => :can"
  (read-from-string (input ":~a" syb-or-string)))
(defun key-to-symbol (key)
  ":can => 'can"
  (read-from-string (subseq (write-to-string key) 1)))

(defun sharpened-symbol (symbol)
  " xx => #'xx (actually 'xx => '#'xx) For converting to function symbol "
  (read-from-string (input "#'~s" symbol)))
(defun quoted-symbol (symbol)
  " xx => 'xx  (actually 'xx => ''xx) "
  (read-from-string (input "'~s" symbol)))

(defun output-symbol (s)
  "Ensure the symbol of macro is still macro of output code..."
  (if+ symbol? quoted-symbol s))


