(in-package :cl-user)
(defpackage usufslc/tests.web
  (:use :cl
        :fiveam
        :cl-mock
        :usufslc.utils
        :usufslc.web
        :usufslc.config
        :usufslc/tests)
  (:export :web-suite))
(in-package :usufslc/tests.web)

(def-suite web-suite
  :description "Unit test suite for the web functions"
  :in usufslc-test-suite)
(in-suite web-suite)

(test app-route-formatting
  :description "Creates a URI from the routing definiton in config"
  (with-mocks ()
    (answer (get-config :section :|app-route|
                        :property :|protocol|)
      "https")
    (answer (get-config :section :|app-route|
                        :property :|port|)
      "12345")
    (answer (get-config :section :|app-route|
                        :property :|host|)
      "usufslc.com")
    (is (equal (usufslc.web::format-app-route "/path") "https://usufslc.com:12345/path"))
    (is (equal (usufslc.web::format-app-route nil) "https://usufslc.com:12345"))))

(test discord-redirect-uri-formatting
  :description "Creates a discord redirect URI from the definition in config"
  ;; This redirect route was generated on the oauth2 > url generator page of discord oauth
  ;; application page (minus client_id ofc)

  ;; We can also guarantee query URI order in quri:
  ;; https://github.com/fukamachi/quri/blob/master/t/encode.lisp#L18-L29
  (with-mocks ()
    (answer (get-config :section :|discord|
                        :property :|auth-url|)
      "https://discord.com/api/oauth2/authorize")
    (answer (get-config :section :|discord|
                        :property :|client-id|)
      "12345")
    (answer (get-config :section :|discord|
                        :property :|scope|)
      "identify email")
    (is (equal (usufslc.web::make-discord-redirect-url "https://usufslc.com/oauth/discord")
               "https://discord.com/api/oauth2/authorize?client_id=12345&redirect_uri=https%3A%2F%2Fusufslc.com%2Foauth%2Fdiscord&response_type=code&scope=identify%20email"))))

