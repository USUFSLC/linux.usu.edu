(in-package :usufslc.web)
(syntax:use-syntax :annot)

@route GET "/announcements" 
(defun get-announcements ()
  (when-session-user-can '(("announce" "announcement"))
    (let ((announcements (mito:select-dao 'usufslc.db.announcement:announcement
                           (sxql:order-by (:desc :scheduled))
                           (mito:includes 'usufslc.db.user::user))))
      (render-with-root #P"announcements/list.lsx"
                        :env `(:announcements ,announcements)
                        :root-env (root-env
                                   :page-title "Announcements")))))
@route GET "/announcements/mine" 
(defun get-my-announcements ()
  (with-authentication-or-sign-in ()
    (let ((announcements (mito:select-dao 'usufslc.db.announcement:announcement
                           (sxql:where (:= :created_by_id (mito:object-id user)))
                           (sxql:order-by (:desc :scheduled))
                           (mito:includes 'usufslc.db.user::user))))
      (render-with-root #P"announcements/list.lsx"
                        :env `(:announcements ,announcements
                               :viewing-my-announcements t)
                        :root-env (root-env
                                   :page-title "My Announcements")))))

@route GET "/announcement/:id" 
(defun view-announcements (&key id)
  (usufslc.db:with-db ()
    (let ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id)))
      (when-session-user-can `(("announce" "announcement")
                               ("read" ,(usufslc.db.announcement::announcement-context announcement)))
          (render-with-root #P"announcements/view.lsx"
                            :env `(:announcement ,announcement))))))

@route GET "/announcement/:id/edit" 
(defun get-announcement-update-form (&key id)
  (usufslc.db:with-db ()
    (let ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id)))
      (when-session-user-can `(("update" ,(usufslc.db.announcement::announcement-context announcement)))
          (render-with-root #P"announcements/form.lsx"
                            :env `(:announcement ,announcement))))))

@route PUT "/announcement/:id" 
(defun update-announcement (&key id name content)
  (usufslc.db:with-db ()
    (let ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id)))
      (when-session-user-can `(("update" ,(usufslc.db.announcement::announcement-context announcement)))
        (redirect-to "/announcements")))))


@route DELETE "/announcement/:id" 
(defun delete-announcement (&key id)
  (usufslc.db:with-db ()
    (let ((announcement (mito:find-dao 'usufslc.db.announcement:announcement :id id)))
      (when-session-user-can `(("delete" ,(usufslc.db.announcement::announcement-context announcement)))
          (redirect-to "/announcements")))))

@route POST "/announcement"
(defun create-announcement (&key name content scheduled)
  (when-session-user-can '(("announce" "announcement"))
    (usufslc.db.announcement:create-announcement-with-announcer-context name content scheduled user)
    (redirect "/announcements")))
