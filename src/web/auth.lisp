(in-package :usufslc.web)

(defun make-discord-redirect-url (usufslc-redirect-path
                                  &key
                                    (discord-oauth-url (get-config-value :|discord| :|auth-url|))
                                    (client-id (get-config-value :|discord| :|client-id|))
                                    (scope (get-config-value :|discord| :|scope|)))
  (render-uri
   (make-uri :defaults discord-oauth-url
             :query `(("client_id" . ,client-id)
                      ("redirect_uri" . ,usufslc-redirect-path)
                      ("response_type" . "code")
                      ("scope" . ,scope)))))

;;(defun retrieve-discord-oauth-token (oauth-code)
;;  (decode-json 
;;   (http-request (config :discord-token-url)
;;                 :method :post
;;                 :parameters `(("client_id" . ,(config :discord-client-id))
;;                               ("scope" . ,(config :discord-scope))
;;                               ("redirect_uri" . ,(format-app-route "/"))
;;                               ("client_secret" . ,(config :discord-client-secret))
;;                               ("grant_type" . "authorization_code")
;;                               ("code" . ,oauth-code))
;;                 :want-stream t)))
;;
;;(defun format-bearer-token-header (discord-oauth-response-alist)
;;  (let ((token-type (cdr (assoc :TOKEN--TYPE discord-oauth-response-alist)))
;;        (access-token (cdr (assoc :ACCESS--TOKEN discord-oauth-response-alist))))
;;    (format nil "~a ~a" token-type access-token)))
;;
;;(defun retrieve-discord-user-details (bearer-token)
;;  (decode-json
;;   (http-request (get-config-value :|discord| :|identity-url|)
;;                 :method :get
;;                 :additional-headers `(("authorization" . ,bearer-token))
;;                 :want-stream t)))
