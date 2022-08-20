(in-package :usufslc)

(defvar *appfile-path*
  (asdf:system-relative-pathname :usufslc #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args)
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* (append
                                         `(:port ,(parse-number (get-config-value :|app| :|port|))
                                           :address ,(get-config-value :|app| :|address|)
                                           :debug ,(get-config-value :|app| :|debug|)
                                           :server :hunchentoot)
                                         args))))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))
