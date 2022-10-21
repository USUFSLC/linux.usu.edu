(ql:quickload :usufslc)

(defpackage usufslc.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :lack.middleware.session.state.cookie
                :make-cookie-state)
  (:import-from :usufslc.web
                :*web*)
  (:import-from :usufslc.config
                :get-config
                :prod-p
                :*static-paths*
                :*application-root*))
(in-package :usufslc.app)

;; Path-expressions is an a-list with (regular-expression-matching-path . path-to-directory) pairs.
(defmacro with-static-handlers (() &body body)
  `(builder
    ,@(mapcar (lambda (path-expression)
                `(:static :path (lambda (path)
                                  (if (scan ,(car path-expression) path)
                                      path))
                          :root (merge-pathnames ,(cdr path-expression) *application-root*)))
              *static-paths*)
    ,@body))

(with-static-handlers ()
  (if (get-config :section :|app-log| :property :|access-log|)
      :accesslog)
  (let ((error-log-path (get-config :section :|app-log| :property :|error-log|)))
    (if error-log-path
        `(:backtrace
          :output ,(pathname error-log-path))))
  `(:session :state ,(make-cookie-state :httponly t :secure t :expires (* 60 60 6)))
  `(:csrf)
  *web*)
