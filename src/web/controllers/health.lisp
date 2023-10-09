(in-package :usufslc.web)
(syntax:use-syntax :annot)

@route GET "/health"
(defun do-health-check ()
  "healthy")
