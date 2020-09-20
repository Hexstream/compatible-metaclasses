(in-package #:compatible-metaclasses)

(defclass compatible-metaclasses:standard-class (compatible-metaclasses:class)
  ())

(defgeneric compatible-metaclasses:metasubstitute (metametaclass)
  (:method ((class cl:class))
    nil))

(defmethod compatible-metaclasses:substitute ((class compatible-metaclasses:standard-class))
  (or (compatible-metaclasses:metasubstitute (class-of class))
      class))


(defclass compatible-metaclasses:validate-as-mixin (class-options:options-mixin)
  ((%validate-as :initarg :validate-as
                 :reader compatible-metaclasses:validate-as
                 :type cl:class
                 :initform (find-class 'cl:standard-class))))

(defun %canonicalize-validate-as (validate-as)
  (find-class (if (typep validate-as '(cons t null))
                  (first validate-as)
                  validate-as)))

(defmethod class-options:canonicalize-options ((class compatible-metaclasses:validate-as-mixin)
                                               &key (validate-as nil validate-as-p))
  (if validate-as-p
      (list* :validate-as (%canonicalize-validate-as validate-as)
             (call-next-method))
      (call-next-method)))


(defclass compatible-metaclasses:metasubstitute-mixin ()
  ((%metasubstitute :reader compatible-metaclasses:metasubstitute
                    #-ccl :type #-ccl cl:class)))

(defun %compute-metasubstitute (class)
  (c2mop:class-prototype (c2mop:ensure-finalized (compatible-metaclasses:validate-as class))))

(defmethod initialize-instance :after ((mixin compatible-metaclasses:metasubstitute-mixin) &key)
  (setf (slot-value mixin '%metasubstitute) (%compute-metasubstitute mixin)))

(defmethod reinitialize-instance :after ((mixin compatible-metaclasses:metasubstitute-mixin) &key)
  (setf (slot-value mixin '%metasubstitute) (%compute-metasubstitute mixin)))


(defclass compatible-metaclasses:standard-metaclass (compatible-metaclasses:validate-as-mixin
                                                     compatible-metaclasses:metasubstitute-mixin
                                                     c2mop:standard-class)
  ())

(define-symbol-macro %standard-object (load-time-value (find-class 'standard-object)))

(defmethod c2mop:validate-superclass ((class compatible-metaclasses:standard-metaclass) (superclass cl:class))
  (c2mop:validate-superclass %standard-object superclass))

(defmethod c2mop:validate-superclass ((class cl:class) (superclass compatible-metaclasses:standard-metaclass))
  (c2mop:validate-superclass class %standard-object))
