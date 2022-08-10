(ql:quickload :usufslc)

(defpackage usufslc.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :usufslc.web
                :*web*)
  (:import-from :usufslc.config
                :config
                :productionp
                :*static-paths*
                :*application-root*))
(in-package :usufslc.app)

;; Path-expressions is an a-list with (regular-expression-matching-path . path-to-file) pairs.
(defmacro with-static-handlers (&rest body)
  `(builder
    ,@(mapcar (lambda (path-expression)
                `(:static :path (lambda (path)
                                  (if (scan ,(car path-expression) path)
                                      path
                                      nil))
                          :root (merge-pathnames ,(cdr path-expression) *application-root*)))
              *static-paths*)
    ,@body))

(with-static-handlers
 (unless (productionp) :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 (unless (productionp)
  (lambda (app)
        (lambda (env)
          (let ((datafly:*trace-sql* t))
            (funcall app env)))))
 *web*)
