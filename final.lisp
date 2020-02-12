(in-package :celwk)

(defun $stop-macro-chars ()
  (mapc '$cancel-macor-char '(#\$ $\%)))
;; (coerce "$%" 'list)
;; ($cancel-macor-char #\$)
;; ($cancel-macor-char #\%)
;; ($stop-macro-chars)

;; (declaim (sb-ext:muffle-conditions cl:warning))  => Disable all warning
;; (declaim (sb-ext:muffle-conditions cl:style-warning)) => Disable only style warning
;; (declaim (sb-ext:unmuffle-conditions cl:warning))
