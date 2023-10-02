(in-package :usufslc.db.context)

(with-db ()
         (defclass context ()
           ((name :col-type (:varchar 128)
                  :accessor context-name))
           (:metaclass mito:dao-table-class)
           (:unique-keys (name)))

         (defclass context-role ()
           ((context :col-type context
                     :accessor context-role-context)
            (name :col-type (:varchar 128)
                  :accessor context-role-name))
           (:metaclass mito:dao-table-class)
           (:unique-keys (context-id name)))

         (defclass context-operation ()
           ((operation :col-type (:varchar 128)
                       :accessor context-operation-name)
            (context :col-type context
                     :accessor context-operation-context))
           (:metaclass mito:dao-table-class)
           (:unique-keys (operation context-id)))

         (defclass context-role-operation ()
           ((context-role :col-type context-role
                          :accessor context-role-operation-context-role)
            (context-operation :col-type context-operation
                               :accessor context-role-operation-context-operation))
           (:metaclass mito:dao-table-class)
           (:unique-keys (context-role-id context-operation-id))))

