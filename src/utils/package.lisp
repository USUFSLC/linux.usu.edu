(in-package :cl-user)
(defpackage usufslc.utils
  (:use :cl
        :parse-number
        :usufslc.config)
  (:export :random-in-range
           :with-exponential-retry))
