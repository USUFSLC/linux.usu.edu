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
