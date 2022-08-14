(in-package :cl-user)
(defpackage usufslc.retry
  (:use :cl
        :usufslc.config)
  (:export :sleep-amount
           :with-exponential-retry))
(in-package :usufslc.retry)

(defparameter *jitter-ms*           1000)
(defparameter *max-retries*         3)
(defparameter *retry-period-ms*     1500)
(defparameter *exponential-backoff* 1.3)

(defun sleep-amount (iteration &key (jitter-ms *jitter-ms*)
                                 (retry-period-ms *retry-period-ms*)
                                 (exponential-backoff *exponential-backoff*))
  (let ((base-sleep (* (expt exponential-backoff (1- iteration)) retry-period-ms))
        (jitter (random jitter-ms)))
    (/ (+ base-sleep jitter) 1000)))


;; Attempts to retry the body call sleeping exponentially between attempts, returning two values:
;; either (nil exception) if all attempts fail, or (value num-attempts) if any attempt's return-value
;; satisfies the validator function
(defmacro with-exponential-retry ((&key
                                     sleep-plist-args
                                     (validator #'identity)
                                     (max-retries *max-retries*))
                                  &body body)
  `(loop for retry from 1 to ,max-retries
         do (let ((current-result (handler-case (progn ,@body)
                                    (error (exception)
                                      (values nil exception)))))
              (if (funcall ,validator current-result)
                  (return (values current-result retry))
                  (sleep (apply 'sleep-amount (cons retry ,sleep-plist-args)))))))
