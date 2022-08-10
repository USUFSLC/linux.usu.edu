(asdf:defsystem "usufslc-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Simponic"
  :license ""
  :depends-on ("usufslc"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "usufslc"))))
  :description "Test system for usufslc"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
