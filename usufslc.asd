(asdf:defsystem "usufslc"
  :version "0.1.0"
  :author "Simponic"
  :license "MIT"
  :depends-on (:clack
               :lack
               :caveman2

               ;; utilities
               :uiop
               :cl-ppcre
               :alexandria
               :parse-number

               ;; DB
               :mito
               :sxql
               :dbi

               ;; URI parsing / decoding
               :quri
               
               ;; templates
               :lsx

               ;; @route annotation
               :cl-syntax-annot
               
               ;; http requests
               :drakma
               
               ;; JSON
               :cl-json)

  :components ((:module "src"
                :components
                ((:module "web"
                  :components 
                  ((:file "web" :depends-on ("discord" "view" "package"))
                   (:file "view" :depends-on ("package"))
                   (:file "package" :depends-on ("discord"))
                   (:module "discord"
                    :components
                    ((:file "auth" :depends-on ("package"))
                     (:file "package"))))
                  :depends-on ("config" "db" "utils"))

                  (:module "db"
                   :components
                   ((:module "models"
                     :depends-on ("db" "package")
                     :components
                     ((:module "user"
                       :depends-on ("context")
                       :components
                       ((:file "extern" :depends-on ("models" "package"))
                        (:file "models" :depends-on ("package"))
                        (:file "package")))
                      (:module "context"
                       :components
                       ((:file "extern" :depends-on ("models" "package"))
                        (:file "models" :depends-on ("package"))
                        (:file "package")))))
                     (:file "db" :depends-on ("package"))
                     (:file "package"))
                    :depends-on ("config"))

                  (:module "utils"
                   :components
                   ((:file "retry_strategy" :depends-on ("package"))
                    (:file "utils" :depends-on ("package"))
                    (:file "package"))
                   :depends-on ("config"))

                  (:module "config"
                   :components
                   ((:file "config" :depends-on ("parser" "package"))
                    (:file "parser" :depends-on ("package"))
                    (:file "package")))

                  (:file "main" :depends-on ("package" "utils" "config" "db" "web"))
                  (:file "package"))))
  :description "A website for the USU Free Software and Linux Club"
  :in-order-to ((test-op (test-op "usufslc/tests"))))

(asdf:defsystem "usufslc/tests"
  :author "Simponic"
  :license "MIT"
  :depends-on (:fiveam
               :cl-mock
               :usufslc)
  :components ((:module "tests"
                :components
                ((:file "db_integ" :depends-on ("suite"))

                 (:file "web" :depends-on ("suite"))
                 (:file "discord" :depends-on ("suite"))
                 (:file "config" :depends-on ("suite"))
                 (:file "utils" :depends-on ("suite"))
                 (:file "suite"))))
  :description "A test suite for USUFSLC"
  :perform (asdf:test-op (o c) (uiop:symbol-call
                                :fiveam :run!
                                (uiop:find-symbol* :usufslc-test-suite :usufslc/tests))))
