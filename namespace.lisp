(in-package #:celwk)

;; (defconstant +mac?+ (not (string-equal "root" (sb-ext:posix-getenv "USER"))))	;; Local Mac: Can/soul-clinic
(defconst +system+ (symbol-to-key (cmd "uname -s")))
(defconst +mac?+ (find :darwin *features*))
(defconst +linux?+ (find :linux *features*))
(defconst +linux-system+ (and +linux?+ (symbol-to-key (last1 (split "	| " (cmd "lsb_release -i"))))))
(defconst +cloud?+ (not +mac?+) "Not in my local Mac")

(defconst +ubuntu?+ (eql :ubuntu +linux-system+) "My cloud server in Huawei")	;; Quit already
(defconst +centos?+ (eql :centos +linux-system+) "My cloud server in Aliyun")

(defconst +home+ (if +cloud?+ "/root/" "/Users/Can/"))
(defconst +projects+ (if +cloud?+ "/root/Projects/" "/Users/Can/Projects/"))
(defconst +which-mac+ (sb-ext:posix-getenv "mac"))

(cond (+mac?+
       (push :mac *features*)
       (push :local *features*)
       (when (string= +which-mac+ "iMac")
         (push :dev *features*)))
      (+ubuntu?+ (push :ubuntu *features*))
      (+centos?+ (push :centos *features*)))

(when +cloud?+
  (push :cloud *features*))

(pushnew :hunchentoot-no-ssl *features*)
(push +projects+ ql:*local-project-directories*)	;; ~/Lisp/mine

