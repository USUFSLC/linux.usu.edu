(in-package :usufslc.web)
(syntax:use-syntax :annot)

@route GET "/"
(defun show-home ()
  (usufslc.db:with-db ()
                      (with-updated-user-model-from-session ()
                                                            (let ((user-name (if user
                                                                               (usufslc.db.user::user-name user)
                                                                               "guest")))
                                                              (render-with-root #P"pages/home.lsx"
                                                                                :root-env (root-env)
                                                                                :env `(:user-name ,user-name
                                                                                                  :meeting-times ,(get-config :section :|club| :property :|meetings|)
                                                                                                  :meeting-place ,(get-config :section :|club| :property :|place|)))))))

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

@route GET "/events"
(defun show-events ()
  (render-with-root #P"pages/events.lsx"
                    :root-env (root-env :page-title "Events")))
