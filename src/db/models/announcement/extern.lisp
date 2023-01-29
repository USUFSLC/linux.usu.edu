(in-package :usufslc.db.announcement)

(defun create-announcer-role-context (context)
  (usufslc.db.context:create-role-in-context context "announcer" usufslc.db.context:*crud-operations*))

(defun create-announcement-with-announcer-context (name content scheduled user)
  (let* ((announcement (mito:create-dao 'usufslc.db.announcement:announcement
                                        :name name
                                        :content content
		                                    :scheduled scheduled
                                        :created-by user))
         (context (mito:create-dao 'usufslc.db.context:context
                                   :name (format nil "announcement-~a"
                                                 (mito:object-id announcement)))))
    (setf (usufslc.db.announcement::announcement-context announcement) context)
    (mito:save-dao announcement)
    (let* ((announcer-role (create-announcer-role-context context))
           (announcer-user-context (mito:create-dao 'usufslc.db.user:user-context
                                                    :user user
                                                    :context context)))
      (mito:create-dao 'usufslc.db.user:user-context-role
                       :user-context announcer-user-context
                       :context-role announcer-role))
    announcement))
