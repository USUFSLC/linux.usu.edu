(in-package :usufslc.db.models)

(with-db ()
  (defclass user ()
    ((name :col-type (:varchar 128)
           :accessor user-name)
     (discord-tag :col-type (:varchar 8)
                  :accessor user-discord-tag)
     (discord-id :col-type (:varchar 128)
                 :accessor user-discord-id))
    (:metaclass mito:dao-table-class)
    '(:unique-keys (:discord-id))))
