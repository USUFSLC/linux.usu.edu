(in-package :cl-user)
(defpackage usufslc.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-paths*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :usufslc.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :usufslc))

(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))
(defparameter *template-root*      "root.lsx")

(defparameter *static-paths*       '(("^(?:/css/|/js/)" . #P"front/dist/")
                                     ("^(?:/users/|/images/|/fonts/|/robot\\.txt$|/favicon.ico$)" . #P"public/")))

(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))