(uiop:define-package #:app/app
  (:use #:cl)
  (:import-from #:reblocks-navigation-widget
                #:defroutes)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:app/pages/login
                #:make-login-page)
  (:import-from #:app/pages/landing
                #:make-landing-page)
  (:import-from #:app/vars
                #:*url-prefix*)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/pages/logout
                #:make-logout-page)
  (:import-from #:app/pages/profiles
                #:make-profiles-widget))
(in-package #:app/app)


(defapp app
  :prefix "/")


(defroutes routes
    ("/login" (make-login-page) )
  ("/logout" (make-logout-page))
  ("/profiles/" (make-profiles-widget))
  ("/" (make-landing-page)))


(defmethod reblocks/session:init ((app app))  
  (make-routes))
