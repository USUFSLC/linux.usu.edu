(in-package :usufslc.config)

(defparameter *app-env-name* "APP_ENV")
(defparameter *app-config-file-env-name* "APP_CONFIG_FILE")

(defparameter *environment* (or (getenv *app-env-name*) "development"))
;; Config globals
(defparameter *application-root* (system-source-directory :usufslc))
(defparameter *config-file* (pathname (or (getenv *app-config-file-env-name*)
                                          (merge-pathnames (concatenate 'string "config/" *environment* ".conf")
                                                           *application-root*))))
(defparameter *config* (parse-config (read-file-lines *config-file*)))

;; Static file regex => path a-list, used to create static handler middleware on the lack application
;; See the "with-static-handlers" macro in app.lisp for more details
(defparameter *static-paths* '(("^(?:/css/|/js/)" . #P"front/dist/")
                               ("^(?:/users/|/images/|/fonts/|/robot\\.txt$|/favicon.ico$)" . #P"public/")))

(defun get-config-value (section-keyword property-keyword &optional (config-map *config*))
  (let ((section (gethash section-keyword config-map)))
    (if section 
        (let ((property (gethash property-keyword section)))
          (unless (equal property "nil")
              property)))))

(defun prod-p ()
  (equal *environment* "production"))
