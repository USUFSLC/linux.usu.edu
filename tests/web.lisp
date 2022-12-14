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
  :description "Unit test suite for the view & routing functions"
  :in usufslc-test-suite)
(in-suite web-suite)

(defmacro with-fake-config-mocks (&body body)
  `(with-mocks ()
     (answer (get-config :section :|app-route|
                         :property :|protocol|)
       "https")
     (answer (get-config :section :|app-route|
                         :property :|port|)
       "12345")
     (answer (get-config :section :|app-route|
                         :property :|host|)
       "usufslc.com")
     (answer (get-config :section :|app-render|
                         :property :|template-directory|)
       "templates/")
     (answer (get-config :section :|app-render|
                         :property :|template-root|)
       "root.lsx")
     ,@body))

(test app-route-formatting
  :description "Creates a URI from the routing definiton in config"
  (with-fake-config-mocks ()
    (is (equal (usufslc.web::format-app-route "/path") "https://usufslc.com:12345/path"))
    (is (equal (usufslc.web::format-app-route nil) "https://usufslc.com:12345"))))

(test authentication-redirection
  :description "Redirection and redirection in session set when user is nil - body executed when user is not nil"
  (with-fake-config-mocks ()
    (let ((caveman2::*session* (make-hash-table))
          (caveman2::*request* (make-instance 'lack.request:request))
          (redirects nil))
      (answer mito:object-id t)
      (answer mito:find-dao t)
      (answer (usufslc.web::format-app-route lack-request) "/bruh-moment")
      (answer (caveman2::redirect url) (push url redirects))
      (usufslc.web::with-authentication-or-sign-in ()
        (error "Should not be called"))
      (is (not (null redirects)))

      (setf redirects nil)
      (setf (gethash :user caveman2::*session*) t)

      (let ((body-called nil))
        (usufslc.web::with-authentication-or-sign-in ()
          (setf body-called t))
        (is (identity body-called))
        (is (equal (gethash :unauth-redirect caveman2::*session*) "/bruh-moment"))
        (is (not (null (gethash :error caveman2::*session*))))
        (is (null redirects))))))

(test render-caches-template-files
  :description "Renders a template file and caches the result"
  (clrhash usufslc.web::*template-registry*)
  (with-fake-config-mocks ()
    (let ((template-file #P"test.lsx"))
      (answer lsx:read-lsx-file (lambda (&key x)
                                  (print x)))
      (loop for i from 1 to 3 do
        (is (equal (render template-file :env '(:x "test"))
                   "test")))
      (is (= 1 (length (invocations 'lsx:read-lsx-file)))))))

(lsx:enable-lsx-syntax)
(test render-with-root-renders-with-root
  :description "Renders a template file with the root template"
  (clrhash usufslc.web::*template-registry*)
  (with-fake-config-mocks ()
    (let ((template-file #P"test.lsx"))
      (if-called 'lsx:read-lsx-file (lambda (path)
                                      (cond
                                        ((equal (pathname-name path) "test")
                                         (lambda (&key name)
                                           <p>Hello, {name}</p>))
                                        ((equal (pathname-name path) "root")
                                         (lambda (&key title content)
                                           <html>
                                           <head>
                                           <title>{title}</title>
                                           </head>
                                           <body>
                                           {content}
                                           </body>
                                           </html>))
                                        (t (unhandled)))))
      (is (equal (mapcar (lambda (s) (string-trim '(#\Space) s))
                  (cl-ppcre:split "\\n" (render-with-root template-file :env '(:name "gamer")
                                                                        :root-env '(:title "Title" :content "HELLO"))))
                 '("<!DOCTYPE html>"
                   "<html>"
                    "<head>"
                      "<title>Title</title>"
                    "</head>"
                    "<body>"
                      "<p>Hello, gamer</p>"
                    "</body>"
                   "</html>"))))))                                                
