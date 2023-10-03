(in-package :usufslc.web)
(syntax:use-syntax :annot)

@route GET "/comments"
(defun show-form ()
  (render-with-root #P"comments/form.lsx"
                    :env `(:csrf-token ,(lack.middleware.csrf:csrf-token *session*)
                                       :hcaptcha-sitekey ,(usufslc.config:get-config :section :|hcaptcha| :property :|sitekey|))
                    :root-env (root-env
                                :page-title "Let Us Know Something")))

@route POST "/comments"
(defun send-comment (&key |comment| |h-captcha-response|)
  (let ((hcaptcha-secret (usufslc.config:get-config :section :|hcaptcha| :property :|secret-key|))
        (hcaptcha-sitekey (usufslc.config:get-config :section :|hcaptcha| :property :|sitekey|))
        (comment-webhook (usufslc.config:get-config :section :|discord| :property :|comment-box-webhook|)))
    (if (and (length |comment|)
             (usufslc.utils:with-exponential-retry
               (:validator (lambda (resp)
                             (cdr (assoc :SUCCESS resp))))

               (decode-json
                 (http-request "https://hcaptcha.com/siteverify"
                               :method :post
                               :parameters `(("response" . ,|h-captcha-response|)
                                             ("secret" . ,hcaptcha-secret))
                               :want-stream t))))
      (progn (http-request comment-webhook
                           :method :post
                           :parameters
                           `(("content" . ,(format nil "Looks like a user made a comment: ~%~a" |comment|)))
                           :additional-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
             (render-with-root #P"comments/accept.lsx"
                               :root-env (root-env :page-title "Submission Accepted")))
      (render-with-root #P"comments/form.lsx"
                        :env `(:hcaptcha-sitekey ,hcaptcha-sitekey)
                        :root-env (root-env :page-title "Failed Request"
                                            :error "Hcaptcha failed or submission was empty, try again")))))

