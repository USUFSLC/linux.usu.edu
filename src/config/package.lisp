(in-package :cl-user)
(defpackage usufslc.config
  (:use :cl)
  (:import-from :cl-ppcre
                :scan
                :register-groups-bind)
  (:import-from :uiop
                :read-file-lines
                :getenv)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :asdf
                :system-source-directory)
  (:export :get-config
           :prod-p
           :dev-p
           :*application-root*
           :*static-paths*))
