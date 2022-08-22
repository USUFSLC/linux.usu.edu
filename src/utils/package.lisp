(in-package :cl-user)
(defpackage usufslc.utils
  (:use :cl)
  (:import-from :parse-number
                :parse-number)
  (:import-from :usufslc.config
                :get-config)
  (:export :random-in-range
           :with-exponential-retry))
