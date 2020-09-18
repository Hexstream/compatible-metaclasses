(cl:defpackage #:compatible-metaclasses_tests
  (:use #:cl #:parachute))

(cl:in-package #:compatible-metaclasses_tests)

(defclass class-mixin (compatible-metaclasses:standard-class)
  ((%slot :reader slot
          :initform 'value)
   (%other-slot :initarg :other-slot
                :reader other-slot))
  (:metaclass compatible-metaclasses:standard-metaclass)
  (:validate-as standard-class))

(defclass my-normal-superclass ()
  ())

(defclass my-special-class (my-normal-superclass)
  ()
  (:metaclass class-mixin)
  (:other-slot other-value))

(defclass my-normal-subclass (my-special-class)
  ()
  (:metaclass class-mixin))


(defclass other-class-mixin (class-mixin)
  ()
  (:metaclass compatible-metaclasses:standard-metaclass)
  (:default-initargs :other-slot 'yet-another-value))

(defclass my-other-special-class (my-special-class)
  ()
  (:metaclass other-class-mixin))


(define-test "main"
  (let ((class (find-class 'my-special-class)))
    (is eq 'value (slot class))
    (is equal '(other-value) (other-slot class)))
  (let ((class (find-class 'my-other-special-class)))
    (is eq 'yet-another-value (other-slot class))))
