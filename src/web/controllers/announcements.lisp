(in-package :usufslc.web)
(syntax:use-syntax :annot)

@route GET "/announcements" 
(defun get-announcements ()
  ;; For now, allow all who can announce view all announcements
  ;; in the future this should be limited to the announcements the
  ;; user has "read" context operation access on
  (when-session-user-can '(("announce" "announcement"))
    (let ((announcements (mito:select-dao 'usufslc.db.announcement:announcement
                           (sxql:order-by (:desc :scheduled))
                           (mito:includes 'usufslc.db.user::user))))
      (render-with-root #P"announcements/list.lsx"
                        :env `(:announcements ,announcements)
                        :root-env (root-env
                                   :page-title "Global Announcements")))))

@route GET "/announcements/mine" 
(defun get-my-announcements ()
  (usufslc.db:with-db ()
    (with-authentication-or-sign-in ()
      (let ((announcements (mito:select-dao 'usufslc.db.announcement:announcement
                             (sxql:where (:= :created_by_id (mito:object-id user)))
                             (sxql:order-by (:desc :scheduled))
                             (mito:includes 'usufslc.db.user::user))))
        (render-with-root #P"announcements/list.lsx"
                          :env `(:announcements ,announcements
                                 :viewing-my-announcements t)
                          :root-env (root-env
                                     :page-title "My Announcements"))))))

@route GET "/announcement/create"
(defun get-announcement-create-form ()
  (when-session-user-can '(("announce" "announcement"))
    (render-with-root #P"announcements/form.lsx"
                      :env `(:csrf-token ,(lack.middleware.csrf:csrf-token *session*))
                      :root-env (root-env
                                 :page-title "Create Announcement"))))

@route GET "/announcement/:id" 
(defun view-announcement (&key id)
  (usufslc.db:with-db ()
    (let* ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id))
           (context (usufslc.db.announcement::announcement-context announcement)))
      (when-session-user-can `(("announce" "announcement")
                               ("read" ,context))
          (render-with-root #P"announcements/view.lsx"
                            :env `(:announcement ,announcement
                                   :csrf-token ,(lack.middleware.csrf:csrf-token *session*)                                                 
                                   :can-edit ,(usufslc.db.user:can user "update" context)
                                   :can-delete ,(usufslc.db.user:can user "delete" context)))))))

@route GET "/announcement/:id/edit" 
(defun get-announcement-update-form (&key id)
  (usufslc.db:with-db ()
    (let ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id)))
      (when-session-user-can `(("update" ,(usufslc.db.announcement::announcement-context announcement)))
          (render-with-root #P"announcements/form.lsx"
                            :env `(:announcement ,announcement
                                   :method "PUT"
                                   :action ,(format nil "/announcement/~a" id)
                                   :csrf-token ,(lack.middleware.csrf:csrf-token *session*)))))))

@route PUT "/announcement/:id" 
(defun update-announcement (&key id |name| |content| |scheduled|)
  (usufslc.db:with-db ()
    (let ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id)))
      (when-session-user-can `(("update" ,(usufslc.db.announcement::announcement-context announcement)))
        (setf (usufslc.db.announcement::announcement-name announcement) |name|
              (usufslc.db.announcement::announcement-scheduled announcement) |scheduled|
              (usufslc.db.announcement::announcement-content announcement) |content|)
        (mito:update-dao announcement)
        (throw-code 200)))))

@route DELETE "/announcement/:id" 
(defun delete-announcement (&key id)
  (usufslc.db:with-db ()
    (let ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id)))
      (when-session-user-can `(("delete" ,(usufslc.db.announcement::announcement-context announcement)))
        (mito:delete-dao announcement)
        (throw-code 200)))))

@route POST "/announcement"
(defun create-announcement (&key |name| |content| |scheduled|)
  (when-session-user-can '(("announce" "announcement"))
    (usufslc.db.announcement:create-announcement-with-announcer-context |name| |content| |scheduled| user)
    (throw-code 200)))
