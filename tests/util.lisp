(in-package :cl-user)
(defpackage usufslc/tests.utils
  (:use :cl
        :usufslc/tests)
  (:export :random-in-range))
(in-package :usufslc/tests.utils)

;; Generate random integers between 0 <= min <= n <= max as determined by
;; :min and :max in a plist
(defun random-in-range (range)
  (let ((min (getf range :min))
        (max (getf range :max)))
    (+ min (random (1+ (- max min))))))
