(in-package :usufslc.db.user)

(defun create-or-update-user-from-discord (discord-user-details)
  (let ((name (cdr (assoc :USERNAME discord-user-details)))
        (discord-tag (cdr (assoc :DISCRIMINATOR discord-user-details)))
        (discord-id (cdr (assoc :ID discord-user-details))))
    (if (and name discord-tag discord-id)
        (with-db ()
          (let ((user (or (mito:find-dao 'user :discord-id discord-id)
                          (make-instance 'user))))
            (setf (user-name user) name
                  (user-discord-id user) discord-id
                  (user-discord-tag user) discord-tag)
            (mito:save-dao user)
            user)))))

(defun can (user operation context)
  t)
