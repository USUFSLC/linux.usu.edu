(in-package :usufslc.web)
(syntax:use-syntax :annot)

@route GET "/stream/create"
(defun create-stream ()
  (when-session-user-can '(("start-stream" "stream"))
    (render-with-root #P"stream/form.lsx"
                      :env `(:csrf-token ,(lack.middleware.csrf:csrf-token *session*))
                      :root-env (root-env
                                 :page-title "New Stream"))))

@route GET "/streams"
(defun render-streams (&key |archived| |year|)
  (usufslc.db:with-db ()
    (let ((streams (mito:select-dao 'usufslc.db.vidstream:vidstream
                     (if |archived|
                         (sxql:where (:and :recorded
                                           (:not :streaming)))
                         (sxql:where :streaming))
                      (sxql:where (:= (:date_part "year" :created_at)
                                      (or |year| (local-time:with-decoded-timestamp (:year y) (local-time:now) y))))
                     (sxql:order-by (:desc :created-at))
                     (mito:includes 'usufslc.db.user::user)))
          (min-year (cadr (assoc :min
                                 (mito:retrieve-by-sql (sxql:select ((:min (:date_part "year" :created_at)))
                                                         (sxql:from :vidstream)))))))
      (render-with-root #P"stream/list.lsx"
                        :root-env (root-env
                                   :page-title (if |archived| "The Archive" "Streams"))
                        :env `(:streams ,streams
                               :archived ,|archived|
                               :min-year ,(when min-year (floor min-year)))))))

@route POST "/stream"
(defun add-stream (&key |name| |description|)
  (when-session-user-can '(("start-stream" "stream"))
    (let ((vidstream (usufslc.db.vidstream:create-stream-with-streamer-context |name| |description| user)))
      (render-with-root #P"stream/instructions.lsx"
                        :root-env (root-env
                                   :page-title "Streaming Instructions")
                        :env `(:stream-url
                               ,(format nil "rtmp://~a/~a"
                                        (usufslc.config:get-config :section :|app-route| :property :|host|)
                                        (usufslc.config:get-config :section :|stream| :property :|rtmp-route|))
                               :stream-name
                               ,(usufslc.db.vidstream::vidstream-name vidstream)
                               :stream-key
                               ,(format nil "~a?token=~a"
                                        (mito:object-id vidstream)
                                        (usufslc.db.vidstream::vidstream-token vidstream)))))))
  
@route GET "/stream/:id"
(defun view-stream (&key id)
  (usufslc.db:with-db ()
    (with-updated-user-model-from-session ()
      (let ((stream (mito:find-dao 'usufslc.db.vidstream:vidstream :id id)))
        (if stream
            (let* ((stream-archived (and (not (usufslc.db.vidstream::vidstream-streaming stream))
                                         (usufslc.db.vidstream::vidstream-recorded stream)))
                   (stream-src
                     (if stream-archived
                         (format nil "/~a/~a.~a"
                                 (get-config :section :|stream| :property :|record-archive-route|)
                                 (mito:object-id stream)
                                 (get-config :section :|stream| :property :|record-archive-format|))
                         (format nil "/~a/~a.m3u8"
                                 (get-config :section :|stream| :property :|hls-route|)
                                 (mito:object-id stream)))))            
              (render-with-root #P"stream/view.lsx"
                                :env `(:stream-src ,stream-src
                                       :stream ,stream
                                       :user ,user
                                       :csrf-token ,(lack.middleware.csrf:csrf-token *session*))
                                :root-env (root-env
                                           :page-title (usufslc.db.vidstream::vidstream-name stream))))
            (throw-code 404))))))

@route POST "/stream/stop/:id"
(defun user-end-stream (&key id)
  (usufslc.db:with-db ()
    (with-authentication-or-sign-in ()
      (let ((stream (mito:find-dao 'usufslc.db.vidstream:vidstream :id id)))
        (when (and stream
                   (can user "stop" (usufslc.db.vidstream::vidstream-context stream)))
          (usufslc.db.vidstream:rotate-token-and-set-streaming stream nil)
          (redirect "/streams"))))))

;; NGINX streaming directives

;; Throw 400 if the stream is not streaming
@route POST "/stream/is_streaming_nginx"
(defun stream-is-streaming (&key |name|)
  (usufslc.db:with-db ()
    (let ((stream (mito:find-dao 'usufslc.db.vidstream:vidstream :id |name|)))
      (if (and stream
               (usufslc.db.vidstream::vidstream-streaming stream))
          "Success"
          (throw-code 400)))))

@route POST "/stream/start_by_token_nginx"
(defun start-stream-via-token (&key |token| |name|)
  (usufslc.db:with-db ()
    (let ((stream (usufslc.db.vidstream:get-stream-unless-expired |token|)))
      (if (and stream
               (= (parse-integer |name|) (mito:object-id stream)))
          (progn
            (setf (usufslc.db.vidstream::vidstream-streaming stream) t)
            (mito:save-dao stream)
            "Started stream")
          (throw-code 400)))))

;; prevent a race condition where nginx might send done_recording while
;; still setting the stream to have ended, causing the recorded field
;; to end up as false when it should be true
(defparameter *update-stream-end-lock* (bt:make-lock))

@route POST "/stream/end_by_token_nginx"
(defun end-stream (&key |token|)
  (bt:with-lock-held (*update-stream-end-lock*)
    (usufslc.db:with-db ()
      (let ((stream (mito:find-dao 'usufslc.db.vidstream:vidstream :token |token|)))
        (when stream
          (usufslc.db.vidstream:rotate-token-and-set-streaming stream nil))))))

@route POST "/stream/done_recording_nginx"
(defun set-recorded (&key |name|)
  (bt:with-lock-held (*update-stream-end-lock*)
    (usufslc.db:with-db ()
      (let ((stream (mito:find-dao 'usufslc.db.vidstream:vidstream :id |name|)))
        (when stream
          (format t "~a" (usufslc.db.vidstream::vidstream-streaming stream))
          (setf (usufslc.db.vidstream::vidstream-recorded stream) t)
          (mito:save-dao stream)
          "Saved recording status")))))
