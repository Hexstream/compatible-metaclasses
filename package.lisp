(cl:defpackage #:compatible-metaclasses
  (:use #:cl)
  (:shadowing-import-from #:enhanced-find-class #:find-class)
  (:shadow #:class
           #:standard-class
           #:substitute)
  (:export #:class
           #:substitute

           #:standard-class
           #:standard-metaclass

           #:validate-as-mixin
           #:validate-as

           #:metasubstitute-mixin
           #:metasubstitute))
