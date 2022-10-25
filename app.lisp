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
  (:import-from :lack.middleware.csrf
                :*lack-middleware-csrf*)
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

(defparameter *csrf-ignored-paths*
  '("/stream/start"
    "/stream/stop"))

(with-static-handlers ()
  (if (get-config :section :|app-log| :property :|access-log|)
      :accesslog)
  (let ((error-log-path (get-config :section :|app-log| :property :|error-log|)))
    (if error-log-path
        `(:backtrace
          :output ,(pathname error-log-path))))
  `(:session :state ,(make-cookie-state :httponly t :secure t :expires (* 60 60 6)))
  (lambda (app) ;; Remove any trailing slashes and ignore CSRF if in *csrf-ignored-paths*
    (lambda (env)
      (let* ((path (getf env :path-info))
             (path-no-trailing-forward (if (and (cl-ppcre:scan "^/.+$" path)
                                                (eq '#\/ (char path (1- (length path)))))
                                           (subseq path 0 (1- (length path)))
                                           path)))             

        ;; Then check if we need to apply CSRF
        (if (member path *csrf-ignored-paths* :test #'equal)
            (funcall app env)
            (funcall
             (funcall *lack-middleware-csrf* app)
             env)))))
  *web*)
