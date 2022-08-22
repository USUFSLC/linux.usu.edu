(in-package :cl-user)
(defpackage usufslc.web
  (:use :cl
        :caveman2)
  (:import-from :usufslc.config
                :*application-root*
                :get-config)
  (:import-from :lsx
                :render-object
                :read-lsx-file)
  (:import-from :quri
                :make-uri
                :render-uri
                :url-encode)
  (:import-from :cl-json
                :decode-json)
  (:import-from :drakma
                :http-request)
  (:export :*web*
           :render
           :render-with-root))
