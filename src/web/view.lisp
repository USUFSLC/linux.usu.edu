(in-package :usufslc.web)

;; A hash table containing template path => template function
(defparameter *template-registry* (make-hash-table :test 'equal))

(defparameter *template-directory* (merge-pathnames (pathname (get-config-value :|app-render| :|template-directory|)) *application-root*))

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
  (render (get-config-value :|app-render| :|template-root|) 
          :env (concatenate 'list
                            `(:content ,(render template-path
                                                :env env
                                                :render-lsx nil))
                            root-env)))
