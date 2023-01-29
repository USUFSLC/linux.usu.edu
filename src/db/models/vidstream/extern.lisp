(in-package :usufslc.db.vidstream)

(defparameter *streamer-operations* '("stop"))

(defun create-stream-token (&optional (len 128))
  (format nil "~X" (crypto:random-bits len)))

(defun create-streamer-role-in-context (context)
  (usufslc.db.context:create-role-in-context context "streamer" *streamer-operations*))

(defun create-stream-with-streamer-context (name description user)
  (let* ((stream (mito:create-dao 'vidstream
                                  :name name
                                  :description description
		                              :streaming nil
                                  :recorded nil
                                  :created-by user
                                  :token (create-stream-token)))
         (context (mito:create-dao 'usufslc.db.context:context
                                   :name (format nil "stream-~a"
                                                 (mito:object-id stream)))))
    (setf (vidstream-context stream) context)
    (mito:save-dao stream)
    (let* ((streamer-role (create-streamer-role-in-context context))
           (streamer-user-context (mito:create-dao 'usufslc.db.user:user-context
                                                   :user user
                                                   :context context)))
      (mito:create-dao 'usufslc.db.user:user-context-role
                       :user-context streamer-user-context
                       :context-role streamer-role))
    stream))

(defun rotate-token-and-set-streaming (stream &optional (streaming t))
  (setf (vidstream-streaming stream) streaming
        (vidstream-token stream) (create-stream-token))
  (mito:save-dao stream))

(defun get-stream-unless-expired (token)
  (let ((stream (mito:find-dao 'vidstream :token token))
        (expiration-time-threshold (parse-number (get-config :section :|stream| :property :|token-expiration|))))
    (if stream
        (unless (local-time:timestamp>
                 (local-time:now)
                 (local-time:timestamp+
                  (slot-value stream 'mito.dao.mixin::created-at)
                  expiration-time-threshold
                  :sec))
          stream))))
