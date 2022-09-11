(in-package :cl-user)
(defpackage usufslc.web
  (:use :cl
        :usufslc.web.discord
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
  (:import-from :cl-ppcre
                :scan)
  (:import-from :cl-json
                :decode-json)
  (:import-from :drakma
                :http-request)
  (:import-from :usufslc.db.user
                :create-or-update-user-from-discord)
  (:export :*web*
           :render
           :render-with-root))
