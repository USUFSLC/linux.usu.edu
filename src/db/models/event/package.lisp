(in-package :cl-user)
(defpackage usufslc.db.event
  (:use :cl
        :mito)
  (:import-from :usufslc.db
    :with-db)
  (:export :event
           :get-unnanounced-events))
