(in-package :cl-user)
(defpackage usufslc.db.user
  (:use :cl
        :mito)
  (:import-from :usufslc.db.context
                :context-role
                :context)
  (:import-from :usufslc.db
                :with-db)
  (:export :user
           :user-context
           :create-or-update-user-from-discord))
