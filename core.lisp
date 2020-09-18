(in-package #:compatible-metaclasses)

(defclass compatible-metaclasses:class (c2mop:standard-class) ())

(defgeneric compatible-metaclasses:substitute (class))

(defmethod c2mop:validate-superclass ((class compatible-metaclasses:class) (superclass cl:class))
  (let ((substitute-class (compatible-metaclasses:substitute class)))
    (if (eq substitute-class class)
        (call-next-method)
        (c2mop:validate-superclass substitute-class superclass))))

(defmethod c2mop:validate-superclass ((class cl:class) (superclass compatible-metaclasses:class))
  (let ((substitute-superclass (compatible-metaclasses:substitute superclass)))
    (if (eq substitute-superclass superclass)
        (call-next-method)
        (c2mop:validate-superclass class substitute-superclass))))
