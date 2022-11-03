(in-package :usufslc.db.vidstream)

(defun create-stream-token (&optional (len 128))
  (format nil "~X" (crypto:random-bits len)))

(defun create-stream (name description)
  (mito:save-dao
   (make-instance 'vidstream
                  :name name
                  :description description
		              :streaming nil
                  :token (create-stream-token))))

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
