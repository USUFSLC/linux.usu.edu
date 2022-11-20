(in-package :usufslc.db.announcement)

(with-db ()
  (defclass announcement ()
    ((name :col-type (:varchar 128)
           :accessor announcement-name)
     (content :col-type :text
              :accessor announcement-content)
     (context :col-type (or context :null)
              :accessor announcement-context)
     (scheduled :col-type :datetime
                :accessor announcement-scheduled)
     (created-by :col-type user
                 :accessor announcement-created-by))
    (:metaclass mito:dao-table-class)))
     
