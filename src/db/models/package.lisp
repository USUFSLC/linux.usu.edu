(in-package :cl-user)
(defpackage usufslc.db.models
  (:use :cl
        :mito)
  (:import-from :usufslc.db
                :with-db)
  (:export :user
           :context))
