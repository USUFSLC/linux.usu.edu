(in-package :cl-user)
(defpackage usufslc.db.context
  (:use :cl
        :mito)
  (:import-from :usufslc.db
                :with-db)
  (:export :context
           :context-operation
           :context-role
           :context-role-operation
           :*crud-operations*
           :create-role-in-context
           :create-operations-around-context))
