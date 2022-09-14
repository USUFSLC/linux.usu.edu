(in-package :usufslc.db.user)

(defun create-or-update-user-from-discord (discord-user-details)
  (let ((name (cdr (assoc :USERNAME discord-user-details)))
        (discord-tag (cdr (assoc :DISCRIMINATOR discord-user-details)))
        (discord-id (cdr (assoc :ID discord-user-details))))
    (if (and name discord-tag discord-id)
        (with-db ()
          (let ((user (or (mito:find-dao 'user :discord-id discord-id)
                          (make-instance 'user))))
            (setf (user-name user) name
                  (user-discord-id user) discord-id
                  (user-discord-tag user) discord-tag)
            (mito:save-dao user)
            user)))))

(defun can (user operation context)
  (with-db ()
    (mito:select-dao 'user-context
      (inner-join :user_context_role :on (:= :user_context_role.user_context_id :user_context.id))
      (inner-join :context_role_operation :on (:= :context_role_operation.context_role_id :user_context_role.context_role_id))
      (inner-join :context_operation :on (:= :context_operation.id :context_role_operation.context_operation_id))
      (where (:and (:= :context_operation.operation operation)
                   (:= :user_context.context_id (object-id context))
                   (:= :user_context.user_id (object-id user)))))))
