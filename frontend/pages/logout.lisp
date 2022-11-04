(uiop:define-package #:app/pages/logout
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:app/vars
                #:*url-prefix*)
  (:import-from #:serapeum
                #:fmt))
(in-package #:app/pages/logout)


(defwidget logout-page ()
  ())


(defun make-logout-page ()
  (make-instance 'logout-page))


(defmethod render ((widget logout-page))
  (log4cl-extras/error:with-log-unhandled ()
    (setf (reblocks/session:get-value :auth-token)
          nil)
    (reblocks/response:set-cookie
     (list :name "auth_token"
           :value ""
           :path "/"
           :expires (get-universal-time)
           :secure t
           :samesite :lax))
    (reblocks/response:redirect (fmt "~A/" *url-prefix*))))

