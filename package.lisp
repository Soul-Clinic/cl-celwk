(defpackage :celwk
  (:use :cl
        :cl-ppcre
        :cl-json
        :sb-concurrency
        :sb-thread)
  (:nicknames := :can :mine :own :kit)
  (:export
   ;; Alias
   #:call
   #:where #:filter
   #:input #:output
   ;; => Basic
   #:progns
   #:@doc
   #:ends-with?
   #:concat-codes

   #:alias
   #:mac
   #:mac+
   #:^
   #:desc
   #:desv
   #:<=>

   ;; => Data
   #:lish :list-hash #:lash
   ;; #:to-json

   #:sesh

   ;; => Debug
   #:manifest

   #:with-time
   #:io

   ;; => File
   #:files-with-suffix
   #:file-size
   #:read-file
   #:write-to-file
   #:org-code
   #:build-orgs
   #:upper-path
   #:update-coding-org
   #:merge-codes-to-orgs


   ;; => Functional
   #:count-general-args
   #:general-args
   #:getf*
   #:arg-list
   #:general-&-keys
   #:previous-matches
   #:memorize~
   #:reversed~
   #:curry~   #:*~  #:**~
   #:compose~ #:<~
   #:pipe~	  #:~>
   #:call-pipe 	  #:<<
   #:call-compose #:>>
   #:always~
   #:is~
   #:bind~
   #:count-args
   #:and~
   #:or~
   #:fetch
   #:function-name

   #:defun! ;; With &key and &rest exclusive
   #:defun+ ;; Define a function with same name function parameter
   #:defun&
   
   #:build-memorized
   #:pipe*
   #:compose*
   #:memorize-curry
   #:~

   ;; => List
   #:repeat
   #:mklist
   #:append1
   #:1-to
   #:0-below
   #:trim-list
   #:suffix-with?
   #:last*
   #:unified-elements?
   #:same-length?
   #:separate-list
   #:average-list
   #:single?
   #:group-by
   #:flat?
   #:flatten1
   #:flatten2
   #:flatten3
   #:deep-remove-if
   #:deep-mapcar
   #:map+
   #:filter+
   #:parallel+
   #:reduce~
   #:remove-duplicate
   #:find-atom+
   #:series
   #:n-times
   #:filled-list

   #:insert*
   #:insert-list
   #:collect-times
   #:self-sort

   ;; => Macro
   #:if*
   #:if~
   #:if+
   #:ifnil!
   #:ifnils!
   
   #:symbol-to-key
   #:symbol-of-symbol
   #:key-to-symbol
   #:sharpened-symbol
   #:quoted-symbol
   #:with-gensyms
   #:defconst
   #:defsymbol
   #:for
   #:in
   #:until
   #:while
   #:fn+
   #:name-lambda
   #:random-choice
   #:nil!
   #:t!
   #:opposite!
   #:self
   #:alias-macro

   ;; => Number
   #:parse-float
   #:scope?


   ;; => Shell
   #:shell
   #:cmd
   ;; #:weplant
   #:psql
   #:program-stream


   ;; => Stream

   #:inspect-file
   #:keep-reading

   ;; => String
   #:empty-string?
   #:string-join
   #:string-repeat
   #:string-of-code
   #:wrapped-code
   #:join
   #:@reglax
   #:replace-string

   #:read-strings
   #:concat #:concat-string #:concat-list #:concat-vector
   #:@replex

   ;; => Style
   #:end->?
   #:bool=>?
   #:end=>?
   #:odd?
   #:even?
   #:number?
   #:zero?
   #:plus?
   #:minus?
   #:bound?
   #:symbol?
   #:fbound?
   #:integer?
   #:float?
   #:complex?
   #:array?
   #:vector?
   #:rational?
   #:real?
   #:alpha-char?
   #:digit-char?
   #:alphanumeric?
   #:list?
   #:cons?
   #:end?
   #:subset?
   #:type?
   #:tail?
   #:hash-table?
   #:constant?
   #:keyword?
   #:string?
   #:pathname?
   #:pathname-match?
   #:wild-pathname?
   #:package?
   #:stream?
   #:input-stream?
   #:interactive-stream?
   #:output-stream?
   #:open-stream?

   #:null? #:never? #:!
   #:atom?
   #:string?
   #:list?
   #:fn? #:function?

   ;; => Syntax
   #:ntimes
   #:flatten
   #:symbol-int
   #:parse-to-lambda
   #:dollar-sign
   #:ddfn

   #:$cancel-dispatch-macor-char
   #:$cancel-macor-char
   #:$stop-macro-chars
   
   #:collect-list
   #:delimit

   ;; => Threads
   #:race
   #:limit-time-call


   ;; => Tools
   #:2d-array-to-list
   #:code-define-list
   #:export-all

   #:ppmx
   #:bound-type
   #:vprint
   #:second-value

   ;; => Trivial
   #:local-time
   #:time-of-universal
   #:now))
