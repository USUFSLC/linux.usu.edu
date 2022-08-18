(in-package :usufslc.web)

(defun retrieve-discord-oauth (oauth-code)
  (decode-json 
   (http-request (config :discord-token-url)
                 :method :post
                 :parameters `(("client_id" . ,(config :discord-client-id))
                               ("scope" . ,(config :discord-scope))
                               ("redirect_uri" . ,*oauth-path*)
                               ("client_secret" . ,(config :discord-client-secret))
                               ("grant_type" . "authorization_code")
                               ("code" . ,|code|))
                 :want-stream t)))

(defun format-bearer-token-header (discord-oauth-response-alist)
  (let ((token-type (cdr (assoc :TOKEN--TYPE discord-oauth-response-alist)))
        (access-token (cdr (assoc :ACCESS--TOKEN discord-oauth-response-alist))))
    (format nil "~a ~a" token-type access-token)))

(defun retrieve-discord-user-details (bearer-token)
  (decode-json
   (http-request (config :discord-user-identity-url)
                 :method :get
                 :additional-headers `(("authorization" . ,bearer-token))
                 :want-stream t)))
