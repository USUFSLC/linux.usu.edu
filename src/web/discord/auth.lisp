(in-package :usufslc.web.discord)

(defun make-discord-redirect-url (usufslc-redirect-path
                                  &key
                                    (discord-oauth-url
                                     (get-config :section :|discord| :property :|auth-url|))
                                    (client-id
                                     (get-config :section :|discord| :property :|client-id|))
                                    (scope
                                     (get-config :section :|discord| :property :|scope|)))
  (render-uri
   (make-uri :defaults discord-oauth-url
             :query `(("client_id" . ,client-id)
                      ("redirect_uri" . ,usufslc-redirect-path)
                      ("response_type" . "code")
                      ("scope" . ,scope)))))

(defun retrieve-discord-token-oauth-response (oauth-code redirect-uri)
  (with-exponential-retry (:validator (lambda (resp)
                                        (every (lambda (field)
                                                 (cdr (assoc field resp)))
                                               '(:ACCESS--TOKEN :TOKEN--TYPE))))
    (decode-json 
      (http-request (get-config :section :|discord| :property :|token-url|)
                    :method :post
                    :parameters `(("client_id" . ,(get-config :section :|discord| :property :|client-id|))
                                  ("scope" . ,(get-config :section :|discord| :property :|scope|))
                                  ("redirect_uri" . ,redirect-uri)
                                  ("client_secret" . ,(get-config :section :|discord| :property :|secret|))
                                  ("grant_type" . "authorization_code")
                                  ("code" . ,oauth-code))
                    :additional-headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                    :want-stream t))))

(defun format-bearer-token-header (discord-oauth-response-alist)
  (let ((token-type (cdr (assoc :TOKEN--TYPE discord-oauth-response-alist)))
        (access-token (cdr (assoc :ACCESS--TOKEN discord-oauth-response-alist))))
    (format nil "~a ~a" token-type access-token)))

(defun retrieve-discord-user-details (bearer-token)
  (with-exponential-retry (:validator (lambda (resp)
                                        (every (lambda (field)
                                                 (cdr (assoc field resp)))
                                               '(:ID :USERNAME :DISCRIMINATOR))))
    (decode-json
      (http-request (get-config :section :|discord| :property :|identity-url|)
                    :method :get
                    :additional-headers `(("authorization" . ,bearer-token))
                    :want-stream t))))
