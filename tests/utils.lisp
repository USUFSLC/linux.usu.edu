(in-package :cl-user)
(defpackage usufslc/tests.utils
  (:use :cl
        :fiveam
        :mockingbird
        :usufslc.utils
        :usufslc/tests)
  (:export :utils-suite))
(in-package :usufslc/tests.utils)

(sb-ext:unlock-package "COMMON-LISP") ;; Unlock package to stub the "random" function

(def-suite utils-suite
  :description "Unit tests for USUFSLC utils"
  :in usufslc-test-suite)
(in-suite utils-suite)

(defparameter *random-attempts* 100)
(defparameter *range* '(:min 0 :max 2))
(test random-in-range-is-always-in-range
  :description "The random-in-range function should always return a number between the given range"
  ;; Firstly generate *random-attempts* random numbers and check that they are all in the range
  (let ((seen-result (make-hash-table)))
    (loop for i from 1
            to *random-attempts*
          do (let ((result (usufslc.utils::random-in-range *range*)))
               (if (and
                    (<= result (getf *range* :max))
                    (>= result (getf *range* :min)))
                   (setf (gethash result seen-result) t))))
    ;; Then check that all the numbers were seen
    (is (every #'identity
               (loop for i from (getf *range* :min)
                       to (getf *range* :max)
                     collect (gethash i seen-result))))))

;; Test retry strategy
(defparameter *max-retries* 3)
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
      (is (= (apply 'usufslc.utils::sleep-exponential-amount args) (/ amount-no-jitter-ms 1000))))
    (with-dynamic-stubs ((random jitter-ms))
      (is (= (apply 'usufslc.utils::sleep-exponential-amount args) (/ amount-max-jitter-ms 1000))))))

(test retry-sleep-amount-range
  :description "Retry sleep amount is always within minimum and maximum for each iteration"
  (is (every #'identity
             (loop for i from (getf *iteration-amount-range* :min)
                     to (getf *iteration-amount-range* :max)
                   collect (with-random-sleep-amount-args (:iteration i)
                             (let ((sleep-amount-sec (apply 'usufslc.utils::sleep-exponential-amount args)))
                               (and (>= sleep-amount-sec (/ amount-no-jitter-ms 1000))
                                    (<= sleep-amount-sec (/ amount-max-jitter-ms 1000)))))))))

(defun dummy (x) nil)
(test failed-validation-function-retried-with-exponential-sleep
  :description "A validation function which always returns nil will have its retry strategy retried max-retry times"
  (with-stubs ((dummy nil))
    (is (null (with-exponential-retry (:max-retries *max-retries*)
                (dummy))))
    (is (= (call-times-for 'dummy) *max-retries*))))
