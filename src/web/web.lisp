(in-package :usufslc.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules
@route GET "/"
(defun show-root ()
  (let* ((user (gethash :user *session*))
         (user-name (if user
                        (usufslc.db.user::user-name user)
                        "guest")))
    (render-with-root #P"pages/home.lsx"
                      :root-env `(:sidebar ,(sidebar-component))
                      :env `(:user-name ,user-name))))

@route GET "/conduct"
(defun show-conduct ()
  (render-with-root #P"pages/conduct.lsx"
                    :root-env `(:page-title "Code of Conduct"
                                :sidebar ,(sidebar-component))))

@route GET "/credits"
(defun show-credits ()
  (render-with-root #P"pages/credits.lsx"
                    :root-env `(:page-title "Credits"
                                :sidebar ,(sidebar-component))))

@route GET "/license"
(defun show-license ()
  (render-with-root #P"pages/license.lsx"
                    :root-env `(:page-title "License"
                                :sidebar ,(sidebar-component))))

@route GET "/oauth/discord" 
(defun save-user-from-discord (&key |code|)
  (if (scan (get-config :section :|discord| :property :|code-validation-regex|) |code|)
      (let ((user-dao 
              (create-or-update-user-from-discord
               (retrieve-discord-user-details
                (format-bearer-token-header
                 (retrieve-discord-token-oauth-response |code|
                                                        (format-app-route (request-path-info *request*))))))))
        (setf (gethash :user *session*) user-dao)
        "Hello")))

@route GET "/login"
(defun login ()
  (render-with-root #P"auth/discord_oauth.lsx"
                    :root-env `(:page-title "Log in with Discord"
                                :sidebar ,(sidebar-component))
                    :env `(:redirect-url ,(make-discord-redirect-url (format-app-route "/oauth/discord")))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render-with-root #P"_errors/404.lsx"))
