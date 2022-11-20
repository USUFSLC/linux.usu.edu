(in-package :usufslc.db.context)

(defparameter *crud-operations* '("create" "read" "update" "delete"))

(defun create-operations-around-context (context operations)
  (let ((context-operations (mapcar (lambda (operation-name)
                                      (mito:create-dao 'usufslc.db.context:context-operation
                                                       :operation operation-name
                                                       :context context))
                                    operations)))
    context-operations))

