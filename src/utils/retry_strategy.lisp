(in-package :usufslc.utils)

(defun sleep-exponential-amount (iteration &key (jitter-ms (parse-number (get-config :section :|retry| :property :|jitter-ms|)))
                                 (retry-period-ms (parse-number (get-config :section :|retry| :property :|period-ms|)))
                                 (exponential-backoff (parse-number (get-config :section :|retry| :property :|exponential|))))
  (let ((base-sleep (* (expt exponential-backoff (1- iteration)) retry-period-ms))
        (jitter (random jitter-ms)))
    (/ (+ base-sleep jitter) 1000)))


;; Attempts to retry the body call sleeping exponentially between attempts, returning two values:
;; either (nil exception) if all attempts fail, or (value num-attempts) if any attempt's return-value
;; satisfies the validator function
(defmacro with-exponential-retry ((&key
                                    sleep-plist-args
                                    (validator #'identity)
                                    (max-retries (parse-number (get-config :section :|retry| :property :|max-retries|))))
                                  &body body)
  `(loop for retry from 1 to ,max-retries
         do (let ((current-result (handler-case (progn ,@body)
                                    (error (exception)
                                      (values nil exception)))))
              (if (funcall ,validator current-result)
                (return (values current-result retry))
                (sleep (apply 'sleep-exponential-amount (cons retry ,sleep-plist-args)))))))

