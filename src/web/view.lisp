(in-package :usufslc.web)

;; A hash table containing template path => template function
(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &key env (render-lsx t))
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (let ((template-file-path (merge-pathnames template-path
                                                 (merge-pathnames (pathname (get-config :section :|app-render| :property :|template-directory|))
                                                                  *application-root*))))
        (setf template (read-lsx-file template-file-path)
              (gethash template-path *template-registry*) template)))
    (let ((object (apply template env)))
      (if render-lsx
          (render-object object nil)
          object))))

(defun render-with-root (template-path &key root-env env)
  (render (get-config :section :|app-render| :property :|template-root|) 
          :env (concatenate 'list
                            `(:content ,(render template-path
                                                :env env
                                                :render-lsx nil))
                            root-env)))

(defun format-app-route (path &key (usufslc-protocol (get-config :section :|app-route| :property :|protocol|))
                                (usufslc-port (get-config :section :|app-route| :property :|port|))
                                (usufslc-host (get-config :section :|app-route| :property :|host|)))
  (render-uri
   (make-uri
    :scheme usufslc-protocol
    :port usufslc-port
    :host usufslc-host
    :path path)))
