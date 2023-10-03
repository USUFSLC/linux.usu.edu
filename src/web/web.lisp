(in-package :usufslc.web)

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defmacro with-updated-user-model-from-session (&body body)
  `(let* ((user-session (gethash :user *session*))
          (user (if user-session
                  (mito:find-dao 'usufslc.db.user::user :id
                                 (mito:object-id user-session)))))
     ,@body))

(defun root-env (&rest other-env)
  (let ((infomsg (gethash :info *session*))
        (errormsg (gethash :error *session*)))
    (setf (gethash :info *session*) nil
          (gethash :error *session*) nil)
    (append other-env
            `(:sidebar
               ,(usufslc.db:with-db
                  ()
                  (with-updated-user-model-from-session
                    ()
                    (render #P"components/sidebar.lsx"
                            :env (list :user user
                                       :can-create-event (and user
                                                              (usufslc.db.user::can-in-context-with-name user "create" "events"))
                                       :can-stream (and user
                                                        (usufslc.db.user::can-in-context-with-name user "start-stream" "stream")))
                            :render-lsx nil)))
               :info ,infomsg
               :error ,errormsg))))

(defmacro with-authentication-or-sign-in (&body body)
  `(with-updated-user-model-from-session
     ()
     (if user
       (progn
         ,@body)
       (progn
         (setf (gethash :unauth-redirect *session*) (format-app-route
                                                      (request-path-info *request*))
               (gethash :error *session*) "You need to login to do that")
         (redirect "/login")))))


;; Error pages
(mapcar (lambda (error-code)
          (defmethod on-exception ((app <web>) (code (eql error-code)))
            (declare (ignore app))
            (render-with-root (pathname (format nil "_errors/~a.lsx" error-code))
                              :root-env (root-env
                                          :page-title "Error"))))
        '(404 403 401 400 500))

