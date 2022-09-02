(in-package :cl-user)

(defpackage usufslc.web.components
  (:use :cl)
  (:import-from :lsx
                :render-object)
  (:import-from :caveman2
                :*session*)
  (:export :sidebar-component))
