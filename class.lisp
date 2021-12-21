(in-package :celwk)

(defun object-fields (object)
  (mapc λ(output "[~A] => ~A~%" (sb-mop:slot-definition-name _)
                (slot-value object (sb-mop:slot-definition-name _)))
        (sb-mop:class-direct-slots (class-of object))))

(defun object-fields (object)
  (dolist (_ (sb-mop:class-direct-slots (class-of object)))
    ($output "[~A] => ~A" (sb-mop:slot-definition-name _)
                (slot-value object (sb-mop:slot-definition-name _)))))


