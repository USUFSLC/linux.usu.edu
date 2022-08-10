(in-package :cl-user)
(defpackage usufslc.web
  (:use :cl
        :caveman2
        :usufslc.config
        :usufslc.view
        :usufslc.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :usufslc.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"root.lsx"))

(defroute "/conduct" ()
  (render-with-root (pathname "pages/conduct.lsx") :root-env '(:page-title "Code of Conduct")))
(defroute "/credits" ()
  (render-with-root (pathname "pages/credits.lsx") :root-env '(:page-title "Credits")))
(defroute "/license" ()
  (render-with-root (pathname "pages/license.lsx") :root-env '(:page-title "License")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render-with-root (pathname "_errors/404.lsx")))
