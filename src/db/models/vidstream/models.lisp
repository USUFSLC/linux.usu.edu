(in-package :usufslc.db.vidstream)

(with-db ()
  (defclass vidstream ()
    ((name :col-type (:varchar 128)
           :accessor stream-name)
     (description :col-type :text
                  :accessor stream-description)
     (token :col-type (:varchar 128)
            :accessor stream-token))
    (:metaclass mito:dao-table-class)))
