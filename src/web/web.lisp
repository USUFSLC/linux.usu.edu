(in-package :usufslc.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Helpers

(defun root-env (&rest other-env)
  (let ((infomsg (gethash :info *session*))
        (errormsg (gethash :error *session*)))
    ;; Reset info and error messages
    (setf (gethash :info *session*) nil
          (gethash :error *session*) nil)
    ;; Return the environment with defaults which would be overridden by other-env (getf returns first occurence)
    (append other-env
            `(:sidebar ,(render #P"components/sidebar.lsx" :env `(:user ,(gethash :user *session*))
                                                           :render-lsx nil)
              :info ,infomsg
              :error ,errormsg))))

(defmacro with-authentication-or-sign-in (&body body)
  `(let* ((user-session (gethash :user *session*))
          (user (if user-session
                    (mito:find-dao 'user :id
                                   (mito:object-id user-session)))))
     (if user
         (progn
           ,@body)
         (progn
           (setf (gethash :unauth-redirect *session*) (format-app-route
                                                       (request-path-info *request*))
                 (gethash :error *session*) "You need to login to do that")
           (redirect "/login")))))


;;
;; Routing rules

;; Static pages

@route GET "/"
(defun show-home ()
  (let* ((user (gethash :user *session*))
         (user-name (if user
                        (usufslc.db.user::user-name user)
                        "guest")))
    (render-with-root #P"pages/home.lsx"
                      :root-env (root-env)
                      :env `(:user-name ,user-name))))

@route GET "/conduct"
(defun show-conduct ()
  (render-with-root #P"pages/conduct.lsx"
                    :root-env (root-env :page-title "Code of Conduct")))

@route GET "/credits"
(defun show-credits ()
  (render-with-root #P"pages/credits.lsx"
                    :root-env (root-env :page-title "Credits")))

@route GET "/license"
(defun show-license ()
  (render-with-root #P"pages/license.lsx"
                    :root-env (root-env :page-title "License")))

;; Auth

@route GET "/oauth/discord" 
(defun save-user-from-discord-and-redirect (&key |code|)
  (if (scan (get-config :section :|discord| :property :|code-validation-regex|) |code|)
      (let ((user-dao 
              (create-or-update-user-from-discord
               (retrieve-discord-user-details
                (format-bearer-token-header
                 (retrieve-discord-token-oauth-response |code|
                                                        (format-app-route (request-path-info *request*)))))))
            (redirect-url (or (gethash :unauth-redirect *session*)
                              "/")))
        (if user-dao
            (progn
              (setf (gethash :user *session*) user-dao
                    (gethash :unauth-redirect *session*) nil)
              (redirect redirect-url))
            (throw-code 500)))
      (throw-code 400)))

@route GET "/login"
(defun login ()
  (render-with-root #P"auth/discord_oauth.lsx"
                    :root-env (root-env
                               :page-title "Log In With Discord")
                    :env `(:redirect-url ,(make-discord-redirect-url (format-app-route "/oauth/discord")))))

@route GET "/logout"
(defun logout ()
  (clrhash *session*)
  (redirect "/"))

;; Streams

@route GET "/stream/create"
(defun admin-portal ()
  (with-authentication-or-sign-in ()
    (render-with-root #P"stream/form.lsx"
                      :root-env (root-env
                                 :page-title "Create a stream"))))

@route GET "/streams"
(defun render-streams ()
  (usufslc.db:with-db ()
    (let ((streams (mito:select-dao 'usufslc.db.vidstream:vidstream
                     (sxql:where
                      (:= :streaming "yes")))))
      (render-with-root #P"stream/list.lsx"
                        :root-env (root-env
                                   :page-title "Streams")
                        :env `(:streams ,streams)))))

@route POST "/stream"
(defun create-stream (&key |name| |description|)
  (with-authentication-or-sign-in ()
    (usufslc.db:with-db ()
      (let ((context (mito:find-dao 'usufslc.db.context:context :name "stream")))
        (if (can user "start-stream" context)
            (usufslc.db.vidstream::vidstream-token
             (usufslc.db.vidstream:create-stream |name| |description|))
            (throw-code 403))))))

@route POST "/stream/start"
(defun start-stream-via-token (&key |token|)
  (usufslc.db:with-db ()
    (let ((stream (usufslc.db.vidstream:get-stream-unless-expired |token|)))
      (if stream
          (progn
            (setf (usufslc.db.vidstream::vidstream-streaming stream) "yes")
            (mito:save-dao stream)
            "Started stream")                  
          (throw-code 400)))))

@route POST "/stream/stop"
(defun end-stream (&key |token|)
  (usufslc.db:with-db ()
    (let ((stream (mito:find-dao 'usufslc.db.vidstream:vidstream :token |token|)))
      (if stream
          (usufslc.db.vidstream:rotate-token-and-set-streaming stream nil)))))

;; 

;;
;; Error pages

(mapcar (lambda (error-code)
          (defmethod on-exception ((app <web>) (code (eql error-code)))
            (declare (ignore app))
            (render-with-root (pathname (format nil "_errors/~a.lsx" error-code)))))
        '(404 403 401 400 500))
