(in-package :cl-user)
(defpackage usufslc.web
  (:use :cl
        :lsx
        :caveman2
        :usufslc.config)
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
