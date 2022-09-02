(in-package :usufslc.web.components)

(lsx:enable-lsx-syntax)

(defun sidebar-component ()
  <a>{(type-of *session*)}</a>)
  
