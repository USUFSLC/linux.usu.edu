(in-package :cl-user)
(defpackage usufslc/tests.retry_strategy
  (:use :cl
        :fiveam
        :mockingbird
        :usufslc.retry
        :usufslc/tests
        :usufslc/tests.utils)
  (:export :retry-strategy-suite))
(in-package :usufslc/tests.retry_strategy)

(def-suite retry-strategy-suite
  :description "Unit tests for retry strategy"
  :in usufslc-test-suite)
(in-suite retry-strategy-suite)

(sb-ext:unlock-package "COMMON-LISP") ;; Unlock package to stub the "random" function

(defparameter *iteration-amount-range* '(:min 1 :max 15)) ;; Number of retries
(defparameter *jitter-ms-range* '(:min 1000 :max 5000))
(defparameter *exponential-backoff-thousandths-range* '(:min 1000 :max 2000))
(defparameter *retry-period-ms-range* '(:min 1000 :max 3000))

(defmacro with-random-sleep-amount-args ((&key iteration) &body body)
  `(let* ((iteration (or ,iteration (random-in-range *iteration-amount-range*)))
          (jitter-ms (random-in-range *jitter-ms-range*))
          (exponential-backoff (/ (random-in-range *exponential-backoff-thousandths-range*) 1000))
          (retry-period-ms (random-in-range *retry-period-ms-range*))
          (args (list iteration :jitter-ms jitter-ms
                                :exponential-backoff exponential-backoff
                                :retry-period-ms retry-period-ms))
          (amount-no-jitter-ms (* (expt exponential-backoff (1- iteration)) retry-period-ms))
          (amount-max-jitter-ms (+ amount-no-jitter-ms jitter-ms)))
     ,@body))

(test retry-sleep-amount
  :description "Retry sleep amount can be minimum or maximum"
  (with-random-sleep-amount-args ()
    (with-dynamic-stubs ((random 0))
      (is (= (apply 'sleep-amount args) (/ amount-no-jitter-ms 1000))))
    (with-dynamic-stubs ((random jitter-ms))
      (is (= (apply 'sleep-amount args) (/ amount-max-jitter-ms 1000))))))

;; Generates truthy or nil value for random sleep-amount for attempts in *iteration-amount-range*
;; and asserts each is non-nil 
(test retry-sleep-amount-range
  :description "Retry sleep amount is consistently within minimum and maximum for iteration"
  (is (every #'identity
             (loop for i from (getf *iteration-amount-range* :min)
                     to (getf *iteration-amount-range* :max)
                   collect (with-random-sleep-amount-args (:iteration i)
                             (let ((sleep-amount-sec (apply 'sleep-amount args)))
                               (and (>= sleep-amount-sec (/ amount-no-jitter-ms 1000))
                                    (<= sleep-amount-sec (/ amount-max-jitter-ms 1000)))))))))
