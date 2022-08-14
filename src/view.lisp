(in-package :cl-user)
(defpackage usufslc.view
  (:use :cl)
  (:import-from :usufslc.config
                :*template-directory*
                :*template-root*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :lsx
                :render-object
                :read-lsx-file)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :reset-template-cache
           :render-with-root
           :render-json))
(in-package :usufslc.view)

;; A hash table containing template path => template function

(defparameter *template-registry* (make-hash-table :test 'equal))
(defun reset-template-cache ()
  (clrhash *template-registry*))

;; Renders a template given path data, its environment p-list, and whether to return the raw
;; LSX template, or to render it to a string (render-lsx == t ? string : lsx-object).
(defun render (template-path &key env (render-lsx t))
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (read-lsx-file (merge-pathnames template-path *template-directory*))
            (gethash template-path *template-registry*) template))
    (let ((object (apply template env)))
      (if render-lsx
          (render-object object nil)
          object))))

(defun render-with-root (template-path &key root-env env)
  (render *template-root* 
          :env (concatenate 'list
                            `(:content ,(render template-path
                                                :env env
                                                :render-lsx nil))
                            root-env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))
