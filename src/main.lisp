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
                                         `(:port ,(parse-number (get-config :section :|app| :property :|port|))
                                           :address ,(get-config :section :|app| :property :|address|)
                                           :debug ,(get-config :section :|app| :property :|debug|)
                                           :server :hunchentoot)
                                         args))))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))
