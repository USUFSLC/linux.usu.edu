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

  (usufslc.scheduled:start-jobs)

  (setf *handler*
        (apply #'clackup *appfile-path*
               (append
                 `(:port ,(parse-number (get-config :section :|app| :property :|port|))
                         :address ,(get-config :section :|app| :property :|address|)
                         :debug ,(get-config :section :|app| :property :|debug|)
                         :server :woo)
                 args))))

(defun stop ()
  (when *handler*
    (clack:stop *handler*)
    (setf *handler* nil)
    (usufslc.scheduled:stop-jobs)))

