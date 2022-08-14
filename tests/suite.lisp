(in-package :cl-user)
(defpackage usufslc/tests
  (:use :cl
        :usufslc
        :fiveam)
  (:export :run!
           :usufslc-test-suite))
(in-package :usufslc/tests)

(def-suite usufslc-test-suite
  :description "The ultimate parent test suite")
