(uiop:define-package #:app/app
  (:use #:cl)
  (:import-from #:reblocks-navigation-widget
                #:defroutes)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks-lass)
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
                #:make-profiles-widget)
  (:import-from #:app/pages/jobs
                #:make-jobs-widget)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/widgets/header
                #:make-page-with-header)
  (:import-from #:app/pages/edit-profile
                #:make-edit-profile-widget)
  (:import-from #:app/pages/chat
                #:make-chat-page))
(in-package #:app/app)


(defapp app
  :prefix "/")


(defroutes routes
    ("/login/" (make-page-with-header
               (make-login-page)) )
  ("/logout/" (make-page-with-header
              (make-logout-page)))
  ("/profiles/" (make-page-with-header
                 (make-profiles-widget)))
  ("/profiles/add/" (make-page-with-header
                     (make-edit-profile-widget)))
  ("/jobs/" (make-page-with-header
             (make-jobs-widget)))
  ("/chat/.*" (make-page-with-header
               (make-chat-page)))
  ("/" (make-page-with-header
        (make-landing-page))))


(defmethod reblocks/session:init ((app app))  
  (make-routes))


(defmethod get-dependencies ((app app))
  (append (list
           (reblocks-lass:make-dependency
             '((body > div)
               :background "#f3f4f7")))
          (call-next-method)))
