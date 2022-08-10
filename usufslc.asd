(asdf:defsystem "usufslc"
  :version "0.1.0"
  :author "Simponic"
  :license "MIT"
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"
               
               ;; For templates
               "lsx"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; for DB
               "datafly"
               "sxql")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "A website for the USU Free Software and Linux Club"
  :in-order-to ((test-op (test-op "usufslc-test"))))
