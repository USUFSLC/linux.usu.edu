(in-package :cl-user)
(defpackage usufslc.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:import-from :uiop
                :getenv)
  (:import-from :asdf
                :system-source-directory)
  (:export :config
           :*application-root*
           :*static-paths*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :usufslc.config)

;; Config globals
(setf (config-env-var) "APP_ENV")
(defparameter *application-root*   (system-source-directory :usufslc))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))
(defparameter *template-root*      "root.lsx")
;; Static file regex => path a-list, used to create static handler middleware on the lack application
;; See the "with-static-handlers" macro in app.lisp for more details
(defparameter *static-paths*       '(("^(?:/css/|/js/)" . #P"front/dist/")
                                     ("^(?:/users/|/images/|/fonts/|/robot\\.txt$|/favicon.ico$)" . #P"public/")))

(let ((database-name             (or (getenv "DATABASE_NAME")        "usufslc"))
      (database-user             (or (getenv "DATABASE_USER")        "postgres"))
      (database-password         (or (getenv "DATABASE_PASSWORD")    "postgres"))
      (database-host             (or (getenv "DATABASE_HOST")        "localhost"))
      (database-port             (or (getenv "DATABASE_PORT")        "5432"))

      (protocol                  (or (getenv "APP_PROTOCOL")         "http"))
      (hostname                  (or (getenv "APP_HOST")             "localhost"))
      (port                      (or (getenv "APP_PORT")             "8080"))

      (discord-oauth-auth-url    (or (getenv "DISCORD_AUTH_URL")     "https://discord.com/api/oauth2/authorize"))
      (discord-token-url         (or (getenv "DISCORD_TOKEN_URL")    "https://discord.com/api/oauth2/token"))
      (discord-user-identity-url (or (getenv "DISCORD_IDENTITY_URL") "https://discord.com/api/users/@me"))
      (discord-scope             (or (getenv "DISCORD_SCOPE")        "identify email"))
      (discord-client-id         (getenv "DISCORD_CLIENT_ID"))
      (discord-client-secret     (getenv "DISCORD_CLIENT_SECRET")))

  (defconfig :common 
    `(:app-uri                   ,(format nil "~a://~a:~a" protocol hostname port)
      :discord-oauth-auth-url    ,discord-oauth-auth-url
      :discord-token-url         ,discord-token-url
      :discord-user-identity-url ,discord-user-identity-url
      :discord-scope             ,discord-scope
      :discord-client-id         ,discord-client-id
      :discord-client-secret     ,discord-client-secret)
  )

  (defconfig |development|
    `(:databases (
      (:maindb                   :postgres 
       :database-name            ,database-name 
       :user-name                ,database-user 
       :password                 ,database-password 
       :host                     ,database-host
       :port                     ,database-port)
      )
    )
  )

  (defconfig |production|
    `(:databases (
      (:maindb                   :postgres 
       :database-name            ,database-name 
       :user-name                ,database-user 
       :password                 ,database-password 
       :host                     ,database-host 
       :port                     ,database-port)
      )
    )
  )

  (defconfig |test|
    `(:databases (
      (:maindb                   :sqlite3 
       :database-name            ":memory:")
      )
    )
  )
)

;; Accessors for application environment
(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))