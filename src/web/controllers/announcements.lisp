(in-package :usufslc.web)
(syntax:use-syntax :annot)

@route GET "/announcements" 
(defun get-announcements ()
  (when-session-user-can "announce" "announcement"
    (let ((announcements (mito:select-dao 'usufslc.db.announcement:announcement
                           (sxql:order-by (:desc :scheduled))
                           (mito:includes 'usufslc.db.user::user))))
      (render-with-root #P"announcements/list.lsx"
                        :env `(:announcements ,announcements)
                        :root-env (root-env
                                   :page-title "Announcements")))))

