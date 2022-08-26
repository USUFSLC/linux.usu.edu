(in-package :cl-user)
(defpackage usufslc/tests.db-integration
  (:use :cl
        :fiveam
        :cl-mock
        :usufslc.utils
        :usufslc.db
        :usufslc.db.user
        :usufslc.db.context
        :usufslc.config
        :usufslc/tests)
  (:export :db-test-integration-suite))
(in-package :usufslc/tests.db-integration)

(def-suite db-test-integration-suite
  :in usufslc-test-suite)
(in-suite db-test-integration-suite)

(test discord-id-unique
  :description "Test that the discord-id is unique"
  (with-db ()
    (let* ((discord-id "12345")
           (user 
             (mito:create-dao 'user
                              :discord-id discord-id
                              :name "Test User"
                              :discord-tag "1234")))
      (is (eq 'unique-violation
              (handler-case (mito:create-dao 'user :discord-id discord-id :name "Test User 2" :discord-tag "4321")
                (dbi.error:dbi-database-error (e)
                  (mito:delete-dao user)
                  'unique-violation)))))))

(test integration-discord-user-models
  :description "Test that user model is created from discord user details, and subsequent
                logins update the model"
  (let ((fake-discord-user-oauth-response
          '((:username . "e")
            (:discriminator . "2718")
            (:id . "1123581321345589")))
        (fake-discord-user-oauth-response-updated
          '((:username . "pi")
            (:discriminator . "3142")
            (:id . "1123581321345589")))
        (user-id 0))
    (create-or-update-user-from-discord fake-discord-user-oauth-response)
    (with-db ()
      (let ((user (mito:find-dao 'user
                                 :name "e"
                                 :discord-tag "2718"
                                 :discord-id "1123581321345589")))
        (setf user-id (mito:object-id user))))
    (create-or-update-user-from-discord fake-discord-user-oauth-response-updated)
    (with-db ()
      (let ((user (mito:find-dao 'user
                                 :name "pi"
                                 :discord-tag "3142"
                                 :discord-id "1123581321345589")))
        (is (equal user-id (mito:object-id user)))
        (mito:delete-dao user)))))
;;
;;(test user-with-role-in-context-can-perform-action
;;  :description "Test that a user with a role in the context can perform an action"
;;  (with-db ()
;;    (let* ((fake-user (mito:create-dao 'user :name "Test User" :discord-tag "1234" :discord-id "12345"))
;;           (fake-context (mito:create-dao 'context))
;;           (fake-read-context-operation (mito:create-dao 'context-operation :context fake-context :operation "view"))
;;           (fake-write-context-operation (mito:create-dao 'context-operation :context fake-context :operation "create"))
;;           (fake-readonly-role (mito:create-dao 'context-role :context fake-context :name "readonly"))
;;           (fake-readwrite-role (mito:create-dao 'context-role :context fake-context :name "readwrite")))
;;      (mito:create-dao 'context-role-operation :context-role fake-readonly-role :context-operation fake-read-context-operation)
;;
;;      (mito:create-dao 'context-role-operation :context-role fake-readwrite-role :context-operation fake-write-context-operation)
;;      (mito:create-dao 'context-role-operation :context-role fake-readwrite-role :context-operation fake-read-context-operation)
    
      
      

