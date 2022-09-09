(in-package :usufslc.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defun root-env (&rest other-env)
  (let ((infomsg (gethash :info *session*))
        (errormsg (gethash :error *session*)))
    (setf (gethash :info *session*) nil
          (gethash :error *session*) nil)
    (append other-env
            `(:sidebar ,(sidebar-component)
              :info ,infomsg
              :error ,errormsg))))

;;
;; Routing rules
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

(defmacro with-authentication-or-sign-in (&rest body)
  `(let ((user (gethash :user *session*)))
     (if user
         (progn ,@body)
         (progn
           (setf (gethash :unauth-redirect *session*)
                 (format-app-route (request-path-info *request*))
                 (gethash :error *session*) "You need to login to do that")
           (redirect "/login")))))

@route GET "/behind-bars"
(defun behind-bars ()
  (with-authentication-or-sign-in 
    "Hello, world!"))

;;
;; Error pages

(mapcar (lambda (error-code)
          (defmethod on-exception ((app <web>) (code (eql error-code)))
            (declare (ignore app))
            (render-with-root (pathname (format nil "_errors/~a.lsx" error-code)))))
        '(404 401 400 500))
