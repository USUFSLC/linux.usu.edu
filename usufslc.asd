(asdf:defsystem "usufslc"
  :version "0.1.0"
  :author "Simponic"
  :license "MIT"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop
               
               ;; for templates
               :lsx

               ;; for @route annotation
               :cl-syntax-annot

               ;; for DB
               :datafly
               :sxql
               
               ;; for http requests
               :drakma
               
               ;; JSON
               :cl-json)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "retry_strategy" :depends-on ("config"))
                 (:file "config"))))
  :description "A website for the USU Free Software and Linux Club"
  :in-order-to ((test-op (test-op "usufslc/tests"))))

(asdf:defsystem "usufslc/tests"
  :author "Simponic"
  :license "MIT"
  :depends-on (:fiveam
               :mockingbird
               :usufslc)
  :components ((:module "tests"
                :components
                ((:file "retry_strategy" :depends-on ("util" "suite"))
                 (:file "util" :depends-on ("suite"))
                 (:file "suite"))))
  :description "A test suite for USUFSLC"
  :perform (asdf:test-op (o c) (uiop:symbol-call
                                :fiveam :run!
                                (uiop:find-symbol* :usufslc-test-suite :usufslc/tests))))
