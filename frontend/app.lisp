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
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/widgets/header
                #:make-page-with-header)
  (:import-from #:app/pages/chat
                #:make-chat-page)
  (:import-from #:app/pages/channel
                #:make-channel-widgets
                #:make-channels-widget))
(in-package #:app/app)


(defapp app
  :prefix "/")


(defroutes routes
    ("/login/" (make-page-with-header
               (make-login-page)) )
  ("/logout/" (make-page-with-header
              (make-logout-page)))
  ("/channels/" (make-page-with-header
                 (make-channels-widget)))
  ("/channels/.*" (make-page-with-header
                   (make-channel-widget)))
  ("/chat/.*" (make-page-with-header
               (make-chat-page)
               :wide t))
  ("/" (make-page-with-header
        (make-landing-page))))


(defmethod reblocks/page:init-page ((app app) url-path expire-at)
  (make-routes))


(defmethod get-dependencies ((app app))
  (list*
   ;; (reblocks-lass:make-dependency
   ;;   '(body
   ;;     :background "rgb(51, 53, 65)"
   ;;     :color "rgb(235, 236, 241)"))
   (call-next-method)))
