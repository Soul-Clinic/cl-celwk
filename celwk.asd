(in-package :cl-user)

(defpackage :celwk-asd
  (:use :cl :asdf))
(in-package :celwk-asd)

;; (require :sb-concurrency)

(defsystem :celwk
  :name "Can's Smart System"
  :version "0.1"
  :author "Can"
  :maintainer "Can EriK Lu"
  :description "Useful tools for every project..."
  :long-description "Save the earth"
  :depends-on (:cl-json
               :sb-concurrency
               :cl-ppcre
               :manifest)
  :serial t
  :components ((:file "package")
               (:file "basic")
               (:file "style")
               (:file "macro")
               (:file "syntax")
               (:file "symbol")
               (:file "string")
               (:file "functional")
               (:file "debug")
               (:file "list")
               (:file "shell")
               (:file "stream")
               (:file "file")
               (:file "tools")
               (:file "number")
               (:file "data")
               (:file "threads")
               (:file "final")))
