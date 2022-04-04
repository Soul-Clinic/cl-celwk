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
               :cl-gd
               :cffi
               :com.gigamonkeys.json)
              ; :manifest)
  :serial t
  :components ((:file "package")
               (:file "basic")               
               (:file "style")
               (:file "macro")
               (:file "syntax")
               (:file "symbol")
               (:file "data")
               (:file "def")
               (:file "string")
               (:file "tools")               
               (:file "functional")
               (:file "debug")
               (:file "list")
               (:file "shell")
               (:file "stream")
               (:file "file")
               (:file "number")
               (:file "threads")
               (:file "math")
               (:file "hash")
               (:file "image")
               (:file "watcher")
               (:file "profiler")
               (:file "trivial")
               (:file "class")
               (:file "process")
               (:file "namespace")
               (:file "final")))
