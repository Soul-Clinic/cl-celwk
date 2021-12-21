(in-package :celwk)

(defun number=>rgb (number)
  (multiple-value-bind (rg b) (truncate number 256)
    (multiple-value-bind (r g) (truncate rg 256)
      (list r g b))))

(defun convert->nearer-rgb (x &optional (max 255) (power 2))
  (values (<< 'round
              (* max ~)
              (- 1 ~)
              (expt ~ power)
              (- 1 (/ x max)))))

(defun nearer-max (value &optional (exp 2) (max 255))
  (round (* max (- 1 (expt (- 1 (/ value max)) exp)))))

(defun to-css-rgb (rgb-list &optional (opacity 0.6))
  ;; (setf (second rgb-list) (nearer-max (second rgb-list) 2))	 ;; Green
  (setf opacity (round (* 255 opacity)))
  (input "~{~2,'0x~}" (nconc rgb-list (list opacity))))

(defun file-type (file)
  (=> (shell-input #/file $file/#)
      (split " " ~)
      (string-downcase (second ~))
      (replace-string ~ "jpeg" "jpg")
      (replace-string ~ "html" "svg")
      (symbol-to-key ~)))

(defun image-size (img)
  "Use `convert' shell app"
  (when (eql (file-type img) :svg)
    (return-from image-size (list 500 500)))

  (let1 (width-height (with-output-to-string (*standard-output*)
                        (shell #/identify -format "%w %h" $img/#)))
    (with-input-from-string (*standard-input* width-height)
      (list (read) (read)))))

;; apt-get install imagemagick
;; yum install imagemagick 
(defun image-average-color (img &aux (type (file-type img)))
  "Use `convert' shell app"
  #| convert 
  # ImageMagick pixel enumeration: 1,1,255,srgb
  0,0: (130,148,86)  #829456  srgb(130,148,86) |#

  (when (eql type :svg)
    (return-from image-average-color "ffffff00"))
  (when (string? img)
    (setf img (string-trim " " img)))
  (handler-case 
      (let1 (info (with-output-to-string (*standard-output*)
                    (shell #/convert "$img" -resize 1x1 txt:-/#)))
        (=> (scan-to-strings #/ s?rgba?\((.*)\)/# info)	;; For CentOS 6  rgba, not srgba
            (second-value ~)
            (elt ~ 0)
            (split "," ~)
            (subseq ~ 0 3)	;; Ignore the opacity
            ;; srgb(43.9526%,61.0026%,61.0026%)[macBook 2019] rgb(194,168,154)   srgb(151,129,109)
            (mapcar (pipe~ λ(ppcre:regex-replace "\\..*$|%" _ "") 'parse-integer 'convert->nearer-rgb) ~)
            (to-css-rgb ~ (if (eql type :jpg) 0.6 0))
            #'string-downcase))
    (error () "66666699")))

(defun image-file-name (file &key (id 0) (platform "Lisp") (sth (pretty-time :date-only t)) max-width)
  "If supplying MAX-WIDTH, will resize this file"
  (destructuring-bind (width height)
      (image-size file)
    
    (when (and max-width (< max-width width))
      (output "Resize from ~Ax~A~%" width height)
      (shell #/convert $file -resize $max-width $file/#)
      (setf height (floor (/ (* max-width height) width)))
      (setf width max-width)
      (output "To: ~Ax~A~%" width height))    
    (values (input "~a-~a-~a-~dx~d-t-~a.~a"
                   platform
                   id
                   sth
                   width
                   height
                   (image-average-color file)
                   (file-type file))
            width
            height)))

(defun replace-filename (filename &optional (from "?") (to "？"))
  ;; (setf filename (replace-string (namestring filename) "?" "\\?" :all t))
  (setf filename (namestring filename))
  (when (search from filename)
    (let ((new-name (replace-string filename from to :all t)))
      (setf new-name (replace-string new-name "\\" "" :all t))
      ;; (vprint (pathname filename) new-name)
      (rename-file (pathname filename) new-name)
      (setf filename (pathname new-name))))
  filename)

;; (defun resize-image (filename save-to &key width height)
;;   "Use shell command `convert' to write an new image"
;;   (cl-gd:with-image-from-file (old filename)
;;     (multiple-value-bind (w h)
;;         (cl-gd:image-size old)
;;       (cond ((notany #'identity (list width height)) (error "Please input WIDTH or HEIGHT"))
;;             ((not width) (setf width (round (* w (/ height h)))))
;;             ((not height) (setf height (round (* h (/ width w))))))

;;       (time (shell #/convert $filename -implode 1 -resize $[width]x$[height] $save-to /#)))))
;; Use convert shell app

(defun scale-at-least (&key origin to (at-least t))
  (let* ((ratio-list (mapcar #'/ to origin))
         (fsort (if at-least #'max #'min))
         (unified-ratio (apply fsort ratio-list)))
    (mapcar λ(* unified-ratio _) origin)))


(defun resize-image (origin &key to (width 200) (height width) (at-least t))
  "Resize the image, keep the orgin width/height ratio"
  (destructuring-bind (owidth oheight) (image-size origin)
    (destructuring-bind (w h)
        (scale-at-least :origin (list owidth oheight) :to (list width height) :at-least at-least)
      (shell #/convert $origin -resize $(round w)x$(round h) $to/#))))
;; (resize-image #p"/Users/Can/Desktop/test.png" :to #p"/Users/Can/Desktop/ooo.png" :width 100)





(defun rename-image (file &key (id 1) (platform "Lisp") (sth (get-internal-real-time))) ;;(get-universal-time)))
  (setf file (replace-filename file "?" "？"))
  (let ((new-name (image-file-name file
                                   :id id
                                   :platform platform
                                   :sth sth)))    
    (rename-file file new-name)))	; new-name maybe only is a name, not including the path, but it works.





(defun rename-images (dir &key (suffix '*) (id 1) (platform "Lisp") (timestamp (get-internal-real-time))) ;;(get-universal-time)))
  "Rename the files in a directory"
  (let ((files (files-with-suffix suffix :dir dir))
        (n 0)
        (results '()))
    (vprint files)
    (dolist (file files results)
      (setf file (replace-filename file "?" "？"))
      (let ((new-name (image-file-name file :id id :platform platform :sth (+ (incf n) timestamp))))
        (rename-file file new-name)
        (pushnew
         (cons (pathname-name file) new-name) results)
        (output "~a => ~a~%" file new-name)))))

;; (time (rename-images "/Users/Can/Desktop/JNT"))
;; (rename-images "~/Desktop/weeks/" :platform "CL")


