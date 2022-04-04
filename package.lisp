(require 'sb-introspect)

(defpackage :celwk
  (:use #:cl
        #:cl-ppcre
        #:cl-json
        #:sb-concurrency
        #:sb-thread)
  (:nicknames := :can :mine :own :kit)
  (:export
   ;; Alias
   #:call
   #:where #:filter
   #:input
   #:fmt
   #:output
   ;; => Namespace
   #:+mac?+
   #:+which-mac+
   #:+cloud?+
   #:+home+
   #:+projects+
   #:+ubuntu?+
   #:+centos+
   #:+linux?+
   #:+linux-system+
   #:+system+
   
   ;; => Basic
   #:progns
   #:@doc
   #:ends-with?
   #:concat-codes
   #:mapcar-with-index
   #:define   
   #:alias
   #:mac
   #:mac+
   #:^
   #:~
   #:_
   #:desc
   #:decc
   #:man
   #:desv
   #:<=>

   ;; => Data
   #:lish :list-hash #:lash
   ;; #:to-json
   #:plist-rows-json
   #:set-time-out
   #:set-interval
   #:clear-last-interval
   #:clear-last-timeout
   
   #:sesh

   ;; => Debug
   #:manifest
   #:$output
   #:$error
   #:output+
   #:with-time
   #:io

   ;; => File
   #:copy-file
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
   #:curry~   #:*~
   #:compose~ 
   #:pipe~	  
   #:call-pipe 	  
   #:call-compose

   #:>> #:-> #:=>
   #:<< #:<-
   #:~>
   #:<~

   #:always~
   #:is~
   #:bind~
   #:count-args
   #:and~
   #:or~
   #:fetch
   #:func-name

   #:@defun ;; With &key and &rest exclusive
   #:defun+ ;; Define a function with same name function parameter
   #:defun&
   #:defun~
   #:defmacro~
   
   #:build-memorized
   #:memorize-curry
   #:!~

   ;; => List
   #:anyone
   #:anysome
   #:repeat
   #:mklist
   #:mkint
   #:assure-list
   #:index-of
   #:append1
   #:1-to
   #:0-below
   #:trim-list
   #:suffix-with?
   #:last1
   #:unified-elements?
   #:same-length?
   #:separate-list
   #:average-list
   #:single?
   #:group-by
   #:flat?
   #:splice
   #:dohash
   #:deep-list-items
   #:find-in-depths
   #:print-list

   ;; #:flatten1
   ;; #:flatten2
   ;; #:flatten3

   #:deep-remove-if
   #:deep-mapcar
   #:deep-map
   #:deep-filter
   #:deep-parallel
   #:deep-find-atom
   
   #:reduce~
   #:remove-duplicate
   #:series
   #:n-times
   #:filled-list

   #:insert
   #:insert*
   #:insert-list
   #:collect-times
   #:self-sort

   ;; => Macro
   #:if*
   #:while*
   #:when*
   #:if~
   #:if+
   #:ifnil!
   #:ifnils!
   #:let1

   ;; => Symbol
   #:symbol-with-keys
   #:symbol-to-key
   #:symbol-of-symbol
   #:key-to-symbol
   #:key-to-string
   #:sharpened-symbol
   #:quoted-symbol
   #:with-gensyms
   #:once-only
   #:defconst
   #:defsymbol
   ;; #:in
   #:until
   #:while
   #:fn+
   #:name-lambda
   #:random-choice
   #:nil!
   #:t!
   #:opposite!
   #:o!
   #:self
   #:alias-macro

   ;; => Number
   #:parse-float
   #:scope?


   ;; => Shell
   #:shell
   #:run
   #:cmd
   ;; #:weplan
   #:shell-input
   #:psql
   #:program-stream


   ;; => Stream

   #:inspect-file
   #:keep-reading

   ;; => String
   #:empty-string?
   #:string-join
   #:split-trim
   #:trim
   #:lisp->camel
   #:symbol->camel
   #:camel->lisp
   #:string-repeat
   #:string-of-code
   #:wrapped-code
   #:join
   #:aesthetic
   #:@reglax	; function
   #:@replex ; macro
   #:replace-string
   #:to-simple-string
   #:insure-string

   #:read-output-string
   #:read-from-format
   #:multiple-json-bind
   #:concat #:concat-string #:concat-list #:concat-vector #:string+

   ;; => Style
   #:notany?
   #:any?
   #:every?
   #:notevery?
   #:some?
   
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
   #:restore-syntax
   #:celwk-syntax
   #:ntimes
   #:flatten
   #:symbol-int
   #:parse-to-lambda
   #:dollar-sign
   #:ddfn
   #:times
   
   #:collect-list
   #:delimit

   ;; => Threads
   #:race
   #:limit-time-call
   #:search-threads


   ;; => Tools
   #:pull-lisp-projects
   #:2d-array-to-list
   #:code-define-list
   #:export-all
   #:assure-json-string!
   #:assure-string!
   #:json-file
   #:upload-images-to-server

   #:ppmx
   #:pmx
   #:bound-type
   #:vprint
   #:second-value

   ;; => Wtacher
   #:watch-file
   #:unwatch-file
   
   ;; => Trivial
   #:toggle!
   #:!!!
   #:machine
   #:local-time
   #:time-of-universal
   #:date-of-universal
   #:now
   #:now*
   #:pretty-time

   ;; => Math
   #:calculus

   ;; => hash
   #:gesh
   #:hash-keys
   #:hash-values
   #:sesh
   #:with-json-object
   #:*debug?*
   #:deep-cars
   #:gesh-bind

   #:file-suffix

   ;; => Images
   #:image-file-name
   #:image-average-color
   #:image-size
   #:resize-image
   #:rename-images
   #:file-type
   #:rename-image
   #:defun
   #:pretty-time
   #:only-time
   #:nearer-max
   #:set.hash
   #:only-time

   ;; => class
   #:object-fields
   ;; => process
   #:save-pid
   #:suicide
   #:set-signal-handler
   
   ))
