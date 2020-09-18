(asdf:defsystem #:compatible-metaclasses_tests

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "compatible-metaclasses unit tests."

  :depends-on ("compatible-metaclasses"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:compatible-metaclasses_tests)))
