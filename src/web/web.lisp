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

@route GET "/test"
(defun test ()
  (format nil "~a" *request*))

;;@route GET "/oauth/discord" 
;;(defun receive-discord-oauth (&key |code|)
  
;;@route GET "/login/discord"
;;(defun login ()
;;  (flet ((encode (string) (url-encode string *drakma-default-external-format*)))
;;    (render-with-root #P"auth/log_in.lsx"
;;                      :root-env '(:page-title "Log In")
;;                      :env `(:redirect-url (make-discord-redirect-url 

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render-with-root #P"_errors/404.lsx"))
