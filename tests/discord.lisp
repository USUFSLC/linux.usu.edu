(in-package :cl-user)
(defpackage usufslc/tests.discord
  (:use :cl
        :fiveam
        :cl-mock
        :usufslc.config
        :usufslc.web.discord
        :usufslc/tests)
  (:export :utils-suite))
(in-package :usufslc/tests.discord)

(def-suite discord-suite
  :description "Unit test suite for the discord module"
  :in usufslc-test-suite)
(in-suite discord-suite)
    
(defmacro with-fake-config-mocks (&body body)
  `(with-mocks ()
     (answer (get-config :section :|discord|
                         :property :|token-url|)
       "https://discord.com/api/oauth2/token")
     (answer (get-config :section :|discord|
                         :property :|auth-url|)
       "https://discord.com/api/oauth2/authorize")
     (answer (get-config :section :|discord|
                         :property :|identity-url|)
       "https://discord.com/api/users/@me")
     (answer (get-config :section :|discord|
                         :property :|client-id|)
       "12345")
     (answer (get-config :section :|discord|
                         :property :|scope|)
       "identify email")
     (answer (get-config :section :|discord|
                         :property :|secret|)
       "secret")))

(test discord-redirect-uri-formatting
  :description "Creates a discord redirect URI from the definition in config"
  ;; This redirect route was generated on the oauth2 > url generator page of discord oauth
  ;; application page (minus client_id ofc)

  ;; We can also guarantee query URI order in quri:
  ;; https://github.com/fukamachi/quri/blob/master/t/encode.lisp#L18-L29
  (with-fake-config-mocks ()
    (is (equal (usufslc.web::make-discord-redirect-url "https://usufslc.com/oauth/discord")
               "https://discord.com/api/oauth2/authorize?client_id=12345&redirect_uri=https%3A%2F%2Fusufslc.com%2Foauth%2Fdiscord&response_type=code&scope=identify%20email"))))

(test discord-oauth-response
  :description "Decodes json of a discord oauth response"
  (with-fake-config-mocks ()
    (let* ((discord-response-json "{\"access_token\":\"12345\",\"token_type\":\"bearer\",\"expires_in\":3600,\"refresh_token\":\"54321\",\"scope\":\"identify email\"}")
           (discord-response-stream (make-string-input-stream discord-response-json)))
      (answer drakma:http-request discord-response-stream)
      (let ((response (usufslc.web::retrieve-discord-oauth-response "random_code" "http://localhost:8080/oauth/discord")))
        (is (equal (cdr (assoc :ACCESS--TOKEN response)) "12345"))
        (is (equal (cdr (assoc :TOKEN--TYPE response)) "bearer"))
        (is (equal (cdr (assoc :EXPIRES--IN response)) 3600))
        (is (equal (cdr (assoc :REFRESH--TOKEN response)) "54321"))
        (is (equal (cdr (assoc :SCOPE response)) "identify email"))))))

(test format-bearer-token-header
  :description "Formats a bearer token header"
  (is (equal (format-bearer-token-header '((:ACCESS--TOKEN . "12345") (:TOKEN--TYPE . "Bearer")))
             "Bearer 12345")))

(test get-discord-user-details
  :description "Decodes json of a discord user details response"
  (with-fake-config-mocks ()
    (let* ((discord-response-json "{\"id\":\"12345\",\"username\":\"test\",\"discriminator\":\"1234\",\"avatar\":\"abcdefg\"}")
           (discord-response-stream (make-string-input-stream discord-response-json)))
      (answer drakma:http-request discord-response-stream)
      (let ((response (retrieve-discord-user-details "Bearer nothing")))
        (is (equal (cdr (assoc :ID response)) "12345"))
        (is (equal (cdr (assoc :USERNAME response)) "test"))
        (is (equal (cdr (assoc :DISCRIMINATOR response)) "1234"))
        (is (equal (cdr (assoc :AVATAR response)) "abcdefg"))))))
  
