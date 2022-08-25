(in-package :cl-user)
(defpackage usufslc.web.discord
  (:use :cl
        :caveman2)
  (:import-from :usufslc.config
                :get-config)
  (:import-from :quri
                :make-uri
                :render-uri
                :url-encode)
  (:import-from :cl-json
                :decode-json)
  (:import-from :drakma
                :http-request)
  (:export :format-bearer-token-header
           :retrieve-discord-token-oauth-response
           :make-discord-redirect-url
           :retrieve-discord-user-details))
