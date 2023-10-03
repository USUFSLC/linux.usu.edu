(in-package :usufslc.scheduled)

(defun send-announcement (event)
  (let ((announcement-webhook (usufslc.config:get-config :section :|discord|
                                                         :property :|announcement-bot-webhook|)))
    (drakma:http-request announcement-webhook
                         :method :post
                         :parameters `(("content" . ,(usufslc.db.event::event-description event)))
                         :additional-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
    (setf (usufslc.db.event::event-announced-at event) (local-time:now))
    (mito:save-dao event)))

(clerk:job-fn "Propagate unsent announcements"
              'every
              `(,30 seconds)
              #'(lambda ()
                  (usufslc.db:with-db
                    ()
                    (let ((events (usufslc.db.event::get-unannounced-events)))
                      (mapcar #'send-announcement events)))))

(defun start-jobs ()
  (format t "beginning scheduled jobs...")
  (clerk:start))
