(in-package :usufslc.db.vidstream)

(with-db ()
  (defclass vidstream ()
    ((name :col-type (:varchar 128)
           :accessor vidstream-name)
     (description :col-type :text
                  :accessor vidstream-description)
     (streaming :col-type (or :boolean :null)
                :accessor vidstream-streaming
                :default nil)
     (stream-context :col-type (or context :null)
                     :accessor vidstream-context)
     (created-by :col-type user
                 :accessor vidstream-created-by)
     (recorded :col-type (or :boolean :null)
               :accessor vidstream-recorded)
     (token :col-type (:varchar 128)
            :accessor vidstream-token))
    (:metaclass mito:dao-table-class)
    (:unique-keys (token))))
