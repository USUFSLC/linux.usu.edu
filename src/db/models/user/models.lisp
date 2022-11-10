(in-package :usufslc.db.user)

(with-db ()
  (defclass user ()
    ((name :col-type (:varchar 128)
           :accessor user-name)
     (is-admin :col-type (or :boolean :null)
               :accessor user-is-admin
               :default nil)
     (discord-avatar  :col-type (:varchar 128)
                      :accessor user-discord-avatar)
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
     (context-role :col-type context-role
                   :accessor user-context-role-context-role))
    (:metaclass mito:dao-table-class)
    (:unique-keys (user-context-id context-role-id))))
