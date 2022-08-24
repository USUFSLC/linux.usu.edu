(in-package :usufslc.db.models)

(with-db ()
  (defclass context () ()
    (:metaclass mito:dao-table-class))

  (defclass context-role ()
    ((context :col-type context
              :accessor context-role-context))
    (:metaclass mito:dao-table-class))

  (defclass context-role-operation ()
    ((operation :col-type (:varchar 255)
                :accessor context-role-operation-operation)
     (context :col-type context
              :accessor context-role-operation-context))
    (:metaclass mito:dao-table-class))

  (defclass user-context ()
    ((user :col-type user
           :accessor user-context-user)
     (context :col-type context
              :accessor user-context-context)
     (context-role :col-type context-role
                   :accessor user-context-context-role))
    (:metaclass mito:dao-table-class)))
