(in-package :usufslc.web)
(syntax:use-syntax :annot)

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