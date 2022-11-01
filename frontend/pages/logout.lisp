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
  (setf (reblocks/session:get-value :auth-token)
        nil)
  (reblocks/response:redirect (fmt "~A/" *url-prefix*)))

