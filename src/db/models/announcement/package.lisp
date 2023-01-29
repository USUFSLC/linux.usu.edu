(in-package :cl-user)
(defpackage usufslc.db.announcement
  (:use :cl
        :mito)
  (:import-from :usufslc.db
                :with-db)
  (:import-from :usufslc.db.context
                :create-operations-around-context
                :context)
  (:import-from :usufslc.db.user
                :user)
  (:export :announcement
           :create-announcement-with-announcer-context))
