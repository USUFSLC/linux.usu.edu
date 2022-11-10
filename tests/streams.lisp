(in-package :cl-user)

(defpackage usufslc/tests.streams
  (:use :cl
        :fiveam
        :cl-mock
        :usufslc.db
        :usufslc.db.vidstream
        :usufslc.web
        :usufslc.config
        :usufslc/tests)
  (:export :streams-suite))
(in-package :usufslc/tests.streams)

(def-suite vidstreams-suite
  :in usufslc-test-suite)
(in-suite vidstreams-suite)
    
(test stream-becomes-expired-after-config-time
  :description "Video streams eventually will fail to authenticate after expiration"
  (let ((stream (make-instance 'vidstream :name "Testing stream"
                                          :description "A description for at testing stream"
                                          :token "testToken"
                                          :created-at (local-time:now))))
    (with-mocks ()
      (answer (usufslc.config:get-config :section :|stream| :property :|token-expiration|) "100")
      (answer mito:find-dao stream)
      (is (get-stream-unless-expired (usufslc.db.vidstream::vidstream-token stream))))
    (with-mocks ()
      (answer (usufslc.config:get-config :section :|stream| :property :|token-expiration|) "1")
      (answer mito:find-dao stream)
      (sleep 1)
      (is (not (get-stream-unless-expired (usufslc.db.vidstream::vidstream-token stream)))))))
