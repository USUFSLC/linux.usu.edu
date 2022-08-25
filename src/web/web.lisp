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
  (render #P"root.lsx"))

@route GET "/conduct"
(defun show-conduct ()
  (render-with-root #P"pages/conduct.lsx" :root-env '(:page-title "Code of Conduct")))

@route GET "/credits"
(defun show-credits ()
  (render-with-root #P"pages/credits.lsx" :root-env '(:page-title "Credits")))

@route GET "/license"
(defun show-license ()
  (render-with-root #P"pages/license.lsx" :root-env '(:page-title "License")))

@route GET "/oauth/discord" 
(defun save-user-from-discord (&key |code|)
  (if |code|
      (let ((user-dao 
              (create-or-update-user-from-discord
               (retrieve-discord-user-details
                (format-bearer-token-header
                 (retrieve-discord-oauth-response |code|
                                                  (format-app-route (request-path-info *request*))))))))
        (format nil "~a" user-dao))))

@route GET "/login/discord"
(defun login ()
  (render-with-root #P"auth/discord_oauth.lsx"
                    :root-env '(:page-title "Log in with Discord")
                    :env `(:redirect-url ,(make-discord-redirect-url (format-app-route "/oauth/discord")))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render-with-root #P"_errors/404.lsx"))
