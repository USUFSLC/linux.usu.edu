(asdf:defsystem "usufslc"
                :version "0.1.0"
                :author "Simponic"
                :license "MIT"
                :depends-on (:clack
                              :lack
                              :lack-middleware-session
                              :lack-middleware-csrf

                              :clack-handler-woo

                              :caveman2

                              ;; utilities
                              :uiop
                              :cl-ppcre
                              :alexandria
                              :parse-number
                              :local-time

                              ;; DB
                              :cl-postgres
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
                              :cl-json

                              ;; scheduling
                              :clerk

                              ;; Threading and locks
                              :bt-semaphore)

                :components ((:module "src"
                                      :components
                                      ((:module "web"
                                                :components
                                                ((:module "controllers"
                                                          :components
                                                          ((:file "auth")
                                                           (:file "pages")
                                                           (:file "comments")
                                                           (:file "event")
                                                           (:file "health")
                                                           (:file "streams"))
                                                          :depends-on ("web" "discord"))
                                                 (:file "web" :depends-on ("discord" "view"))
                                                 (:file "view" :depends-on ("package"))
                                                 (:file "package" :depends-on ("discord"))
                                                 (:module "discord"
                                                          :components
                                                          ((:file "auth" :depends-on ("package"))
                                                           (:file "package"))))
                                                :depends-on ("config" "db" "utils" "scheduled"))
                                       (:module "scheduled"
                                                :depends-on ("db")
                                                :components
                                                ((:file "jobs" :depends-on ("package"))
                                                 (:file "package")))
                                       (:module "db"
                                                :components
                                                ((:module "seeds"
                                                          :depends-on ("models")
                                                          :components
                                                          ((:file "contexts" :depends-on ("package"))
                                                           (:file "package")))
                                                 (:module "models"
                                                          :depends-on ("db")
                                                          :components
                                                          ((:module "user"
                                                                    :depends-on ("context")
                                                                    :components
                                                                    ((:file "extern" :depends-on ("models"))
                                                                     (:file "models" :depends-on ("package"))
                                                                     (:file "package")))
                                                           (:module "context"
                                                                    :components
                                                                    ((:file "extern" :depends-on ("models"))
                                                                     (:file "models" :depends-on ("package"))
                                                                     (:file "package")))
                                                           (:module "vidstream"
                                                                    :components
                                                                    ((:file "extern" :depends-on ("models"))
                                                                     (:file "models" :depends-on ("package"))
                                                                     (:file "package")))
                                                           (:module "event"
                                                                    :depends-on ("context" "user")
                                                                    :components
                                                                    ((:file "extern" :depends-on ("models"))
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
                                                ((:file "config" :depends-on ("parser"))
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
                                       (:file "streams" :depends-on ("suite"))
                                       (:file "web" :depends-on ("suite"))
                                       (:file "discord" :depends-on ("suite"))
                                       (:file "config" :depends-on ("suite"))
                                       (:file "utils" :depends-on ("suite"))
                                       (:file "suite"))))
                :description "A test suite for USUFSLC"
                :perform (asdf:test-op (o c) (uiop:symbol-call
                                               :fiveam :run!
                                               (uiop:find-symbol* :usufslc-test-suite :usufslc/tests))))
