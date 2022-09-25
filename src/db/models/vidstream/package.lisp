(in-package :cl-user)
(defpackage usufslc.db.vidstream
  (:use :cl
        :mito)
  (:import-from :usufslc.db
                :with-db)
  (:export :vidstream))
