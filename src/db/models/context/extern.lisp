(in-package :usufslc.db.context)

(defparameter *crud-operations* '("create" "read" "update" "delete"))

(defun create-operations-around-context (context operations)
  (let ((context-operations (mapcar (lambda (operation-name)
                                      (mito:create-dao 'usufslc.db.context:context-operation
                                                       :operation operation-name
                                                       :context context))
                                    operations)))
    context-operations))

(defun create-role-in-context (context name operations)
  (let ((role (mito:create-dao 'usufslc.db.context:context-role
                               :context context
                               :name name))
        (role-operations (create-operations-around-context context operations)))
    (mapcar (lambda (operation)
              (mito:create-dao 'usufslc.db.context:context-role-operation
                               :context-role role
                               :context-operation operation))
            role-operations)
    role))

(defmacro with-objects-where-user-has-operation-in-context-q ((dao-class user operation &optional query) &body body)
  `(let ((objects
           (if (usufslc.db.user::user-is-admin ,user)
               (mito:select-dao ,dao-class ,@query)
               (mito:select-dao ,dao-class
                 (sxql:inner-join :user_context :on (:= :user_context.context_id :context_id))
                 (sxql:inner-join :user_context_role :on (:= :user_context_role.user_context_id :user_context.id))
                 (sxql:inner-join :context_operation :on (:= :context_operation.id :context_role_operation.context_operation_id))
                 (sxql:where (:and (:= :context_operation.operation ,operation)
                                   (:= :user_context.user_id (mito:object-id ,user))))))))
     ,@body))
