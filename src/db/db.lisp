(in-package :usufslc.db)
(setf mito:*auto-migration-mode* (or (not (null
                                            (get-config :section :|db| :property :|auto-migrate|)))
                                     (dev-p))
      mito:*mito-logger-stream* (or (not (null
                                           (get-config :section :|app-log| :property :|sql-info|)))
                                    (dev-p)))

(defparameter *connection-args*
  (list (get-config :section :|db| :property :|driver|)
        :database-name (get-config :section :|db| :property :|name|)
        :username (get-config :section :|db| :property :|username|)
        :password (get-config :section :|db| :property :|password|)
        :host (get-config :section :|db| :property :|host|)
        :port (parse-number (get-config :section :|db| :property :|port|))))

(defmacro with-db ((&key (connection-args *connection-args*)) &body body)
  `(if (not mito:*connection*)
     (let ((mito:*connection* (connect-cached ,@connection-args)))
       (unwind-protect (progn ,@body)
         (disconnect mito:*connection*)))
     (progn ,@body)))

