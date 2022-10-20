(in-package :cl-user)
(defpackage usufslc.db.vidstream
  (:use :cl
        :mito)
  (:import-from :usufslc.db
                :with-db)
;;  (:import-from :cl-schedule
;;                :schedule!)
  (:import-from :parse-number
                :parse-number)
  (:import-from :usufslc.config
                :get-config)
  (:export :vidstream
           :create-stream
           :get-stream-unless-expired
           :rotate-token-and-set-streaming))
