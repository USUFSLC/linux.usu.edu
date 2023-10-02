(in-package :cl-user)
(defpackage usufslc.db.vidstream
  (:use :cl
        :mito)
  (:import-from :usufslc.db
    :with-db)
  (:import-from :usufslc.db.user
    :user)
  (:import-from :usufslc.db.context
    :context)
  (:import-from :parse-number
    :parse-number)
  (:import-from :usufslc.config
    :get-config)
  (:export :vidstream
           :create-stream-with-streamer-context
           :get-stream-unless-expired
           :rotate-token-and-set-streaming))

