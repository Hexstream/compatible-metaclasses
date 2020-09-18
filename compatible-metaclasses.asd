(asdf:defsystem #:compatible-metaclasses

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "Validates superclasses according to a simple substitution model, thereby greatly simplifying the definition of class mixins."

  :depends-on ("closer-mop"
               "enhanced-find-class"
               "class-options")

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
               (:file "core")
               (:file "standard"))

  :in-order-to ((asdf:test-op (asdf:test-op #:compatible-metaclasses_tests))))
