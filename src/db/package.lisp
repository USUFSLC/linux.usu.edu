(in-package :cl-user)
(defpackage usufslc.db
  (:use :cl)
  (:import-from :usufslc.config
                :get-config)
  (:import-from :dbi
                :connect-cached)
  (:import-from :mito
                :deftable)
  (:export :with-connection))
