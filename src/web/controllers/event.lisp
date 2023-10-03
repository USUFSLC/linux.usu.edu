(in-package :usufslc.web)
(syntax:use-syntax :annot)

(defun month-start ()
  (local-time:adjust-timestamp (local-time:today) (offset :month 0)))

(defun month-end ()
  (local-time:adjust-timestamp (local-time:today) (offset :month 1)))

(defun encode-event (event)
  (let ((created-by (usufslc.db.event::event-created-by event)))
    `((id . ,(mito:object-id event))
      (start . ,(local-time:format-timestring nil (usufslc.db.event::event-start-at event)))
      (end . ,(local-time:format-timestring nil (usufslc.db.event::event-end-at event)))
      (name . ,(usufslc.db.event::event-name event))
      (description . ,(usufslc.db.event::event-description event))
      (createdby . ,(usufslc.db.user::user-name created-by)))))

;; todo: sanitize dates
@route GET "/api/events"
(defun get-events (&key (|start| (month-start)) (|end| (month-end)))
  (usufslc.db:with-db
    ()
    (let ((events
            (mito:select-dao 'usufslc.db.event:event
                             (mito:includes 'usufslc.db.user:user)
                             (sxql:where (:and (:<= :start-at |end|)
                                               (:>= :end-at |start|))))))
      (cl-json:encode-json-to-string
        (mapcar #'encode-event events)))))

@route GET "/event/create"
(defun create-new-event (&key id)
  (usufslc.db:with-db
    ()
    (with-authentication-or-sign-in
      ()
      (if (usufslc.db.user::can-in-context-with-name user "create" "events")
        (render-with-root #P"event/form.lsx"
                          :env `(:csrf-token ,(lack.middleware.csrf:csrf-token *session*))
                          :root-env (root-env
                                      :page-title "Create Event"))
        (throw-code 403)))))

@route GET "/event/:id/delete"
(defun delete-event (&key id)
  (usufslc.db:with-db
    ()
    (with-authentication-or-sign-in
      ()
      (let ((event (mito:find-dao 'usufslc.db.event:event :id id)))
        (if event
          (if (usufslc.db.user::can user "destrooy" (usufslc.db.event::event-context event))

            (progn
              (mito:delete-dao event)
              (setf (gethash :info *session*) "deleted event")
              (redirect "/"))
            (throw-code 403))
          (throw-code 404))))))

@route GET "/event/:id/edit"
(defun update-event-form (&key id)
  (usufslc.db:with-db
    ()
    (with-authentication-or-sign-in
      ()
      (let ((event (mito:find-dao 'usufslc.db.event:event :id id)))
        (if event
          (if (usufslc.db.user::can user "update" (usufslc.db.event::event-context event))

            (render-with-root #P"event/form.lsx"
                              :env `(:event ,event
                                            :csrf-token ,(lack.middleware.csrf:csrf-token *session*))
                              :root-env (root-env
                                          :page-title "Update Event"))
            (throw-code 403))
          (throw-code 404))))))

@route GET "/event/:id"
(defun view-event (&key id)
  (usufslc.db:with-db
    ()
    (with-updated-user-model-from-session
      ()
      (let ((event (mito:find-dao 'usufslc.db.event:event :id id)))
        (if event
          (render-with-root #P"event/view.lsx"
                            :env `(:event ,event
                                          :user ,user)
                            :root-env (root-env
                                        :page-title (usufslc.db.event::event-name event)))
          (throw-code 400))))))


@route POST "/event"
(defun add-event (&key |id| |name| |description| |announce-at| |starts-at| |ends-at|)
  (usufslc.db:with-db
    ()
    (with-authentication-or-sign-in
      ()
      (let ((event (if (not (equal |id| ""))
                     (mito:find-dao 'usufslc.db.event:event :id |id|))))
        (if (and
              (usufslc.db.user::can-in-context-with-name user "create" "events")
              (if event
                (usufslc.db.user::can user "update"
                                      (usufslc.db.event::event-context event))
                t))
          (let* ((new-event
                   (if event
                     (progn
                       (setf (usufslc.db.event::event-name event) |name|
                             (usufslc.db.event::event-description event) |description|
                             (usufslc.db.event::event-announce-at event) (if (not (equal |announce-at| ""))
                                                                           |announce-at|)
                             (usufslc.db.event::event-start-at event) |starts-at|
                             (usufslc.db.event::event-end-at event) |ends-at|)
                       event)
                     (usufslc.db.event::create-event-with-creator-context
                       |name| |description| user |starts-at| |ends-at|
                       (if (not (equal |announce-at| ""))
                         |announce-at|)))))

            (mito:save-dao new-event)

            (setf (gethash :info *session*) "Successfully saved event")
            (redirect (format nil "/event/~a/edit"
                              (mito:object-id new-event)))))))))
