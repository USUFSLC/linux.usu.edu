(in-package :usufslc.db.user)

(with-db ()
  (defclass user ()
    ((name :col-type (:varchar 128)
           :accessor user-name)
     (discord-tag :col-type (:varchar 8)
                  :accessor user-discord-tag)
     (discord-id :col-type (:varchar 128)
                 :accessor user-discord-id))
    (:metaclass mito:dao-table-class)
    (:unique-keys (discord-id)))

  (defclass user-context ()
    ((user :col-type user
           :accessor user-context-user)
     (context :col-type user
              :accessor user-context-context))
    (:metaclass mito:dao-table-class))

  (defclass user-context-role ()
    ((user-context :col-type user-context
                   :accessor user-context-role-user-context)
     (role :col-type context-role 
           :accessor user-context-role-role))
    (:metaclass mito:dao-table-class)))

