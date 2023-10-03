(in-package :usufslc.db.event)

(with-db ()
         (defclass event ()
           ((name :col-type (:varchar 128)
                  :accessor event-name)
            (description :col-type :text
                         :accessor event-description)
            (event-context :col-type (or usufslc.db.context:context :null)
                           :accessor event-context)
            (announce-at :col-type (or :timestamptz :null)
                         :accessor event-announce-at)
            (start-at :col-type :timestamptz
                      :accessor event-start-at)
            (end-at :col-type :timestamptz
                    :accessor event-end-at)
            (announced-at :col-type (or :timestamptz :null)
                          :accessor event-announced-at)
            (created-by :col-type usufslc.db.user:user
                        :accessor event-created-by))
           (:metaclass mito:dao-table-class))

         (mito:execute-sql
           (sxql:create-index :end_at_idx
                              :on '(:event :end_at)))
         (mito:execute-sql
           (sxql:create-index :start_at_idx
                              :on '(:event :start_at)))
         (mito:execute-sql
           (sxql:create-index :announced_at_idx
                              :on '(:event :announced_at)))
         (mito:execute-sql
           (sxql:create-index :announce_at_idx
                              :on '(:event :announce_at))))
