(in-package :usufslc.db.seeds)

(defun seed-streaming-context ()
  (with-db
    ()
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


(defun seed-events-context ()
  (with-db
    ()
    (let* ((events-context (mito:create-dao 'usufslc.db.context:context
                                            :name "events"))
           (create-event-operation (mito:create-dao 'usufslc.db.context:context-operation
                                                    :context events-context
                                                    :operation "create"))
           (can-create-event-role (mito:create-dao 'usufslc.db.context:context-role
                                                   :name "can-create-events"
                                                   :context events-context))
           (can-create-event-role-operation (mito:create-dao 'usufslc.db.context:context-role-operation
                                                             :context-role can-create-event-role
                                                             :context-operation create-event-operation))))))

