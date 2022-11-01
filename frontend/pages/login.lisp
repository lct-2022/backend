(uiop:define-package #:app/pages/login
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/vars
                #:*url-prefix*)
  (:import-from #:passport/client
                #:make-passport)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:app/forms
                #:with-html-form))
(in-package #:app/pages/login)


(defwidget login-page ()
  ())


(defun make-login-page ()
  (make-instance 'login-page))


(defmethod render ((widget login-page))
  (flet ((login-user (&key email password &allow-other-keys)
           (log:info "Logging user in" email password)
           (handler-case
               (let* ((client (passport/client::connect (make-passport)))
                      (token (passport/client::login client email password)))
                 (setf (reblocks/session:get-value :auth-token)
                       token)
                 (reblocks/response:redirect (fmt "~A/" *url-prefix*)))
             (rpc-error (c)
               (reblocks-ui/form:form-error (rpc-error-message c))))))
    (with-html
      (with-html-form (:post #'login-user)
        (:input :type "text"
                :name "email"
                :placeholder "Ваш email")
        (:input :type "password"
                :name "password"
                :placeholder "Ваш пароль")
        (reblocks-ui/form:form-error-placeholder)
        
        (:input :type "submit"
                :class "button"
                :value "Войти")))))


(defmethod get-dependencies ((widget login-page))
  (list
   (reblocks-lass:make-dependency
     '(.login-page
       :width 50%
       :margin-left auto
       :margin-right auto))))
