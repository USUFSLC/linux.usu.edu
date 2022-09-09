(in-package :usufslc.web.components)

(lsx:enable-lsx-syntax)

(defun sidebar-component ()
  (let* ((user (gethash :user *session*))
         (user-name (if user (usufslc.db.user::user-name user))))
    <div>
      <a id="theme-switcher"><img id="theme-icon" src="/images/sun.svg" alt="Theme picker"></a>
      <hr>
      <ul>
        {
          (if user
              <div>
                <p>Signed in as {user-name}</p>
                <a href="/logout">Logout</a>
              </div>
              <a href="/login">Login</a>)
        }
      </ul>
      <hr>
    </div>))
  
