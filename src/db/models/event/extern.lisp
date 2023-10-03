(in-package :usufslc.db.event)

(defparameter *creator-operations* '("update" "destroy"))

(defun get-unannounced-events ()
  (let ((now (local-time:now)))
    (with-db ()
             (mito:select-dao 'usufslc.db.event:event
                              (sxql:where (:and
                                            (:is-null :announced-at)
                                            (:<= :announce-at now)))))))

(defun create-creator-role-in-context (context)
  (let ((creator-role (mito:create-dao 'usufslc.db.context:context-role
                                       :context context
                                       :name "event"))
        (creator-operations (mapcar (lambda (operation-name)
                                      (mito:create-dao 'usufslc.db.context:context-operation
                                                       :operation operation-name
                                                       :context context))
                                    *creator-operations*)))
    (mapcar (lambda (creator-operation)
              (mito:create-dao 'usufslc.db.context:context-role-operation
                               :context-role creator-role
                               :context-operation creator-operation))
            creator-operations)
    creator-role))

(defun create-event-with-creator-context (name description creator start-at end-at &optional (announce-at nil))
  (let* ((event (mito:create-dao 'event
                                 :name name
                                 :description description
                                 :created-by creator
                                 :start-at start-at
                                 :end-at end-at
                                 :announce-at announce-at
                                 :announced-at nil))
         (context (mito:create-dao 'usufslc.db.context:context
                                   :name (format nil "event-~a"
                                                 (mito:object-id event)))))
    (setf (event-context event) context)
    (mito:save-dao event)
    (let* ((event-creator-role (create-creator-role-in-context context))
           (event-creator-context (mito:create-dao 'usufslc.db.user:user-context
                                                   :user creator
                                                   :context context)))
      (mito:create-dao 'usufslc.db.user:user-context-role
                       :user-context event-creator-context
                       :context-role event-creator-role))
    event))
