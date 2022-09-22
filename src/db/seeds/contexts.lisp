(in-package :usufslc.db.seeds)

(defun seed-streaming-context ()
  (with-db ()
    (let* ((stream-context (mito:create-dao 'usufslc.db.context:context
                                            :name "stream"))
           (start-stream-operation (mito:create-dao 'usufslc.db.context:context-operation
                                                    :context stream-context
                                                    :operation "start-stream"))
           (can-stream-role (mito:create-dao 'usufslc.db.context:context-role
                                             :name "can-stream"
                                             :context stream-context))
           (can-stream-role-operation (mito:create-dao 'usufslc.db.context:context-role-operation
                                                       :context-role can-stream-role
                                                       :context-operation start-stream-operation))))))
