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
                #:make-channel-widget)
  (:import-from #:app/pages/channels
                #:make-channels-widget)
  (:import-from #:reblocks/page
                #:render-headers)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-prometheus
                #:prometheus-app-mixin)
  (:import-from #:app/widgets/test-page
                #:make-test-page))
(in-package #:app/app)


(defapp app
  :subclasses (prometheus-app-mixin)
  :prefix "/")


(defroutes routes
    ("/login/" (make-page-with-header
                (make-login-page)) )
  ("/logout/" (make-page-with-header
               (make-logout-page)))
  ("/test/" (make-test-page))
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

;; TODO: раскомментировать, чтобы мерять производительность
;; (defmethod reblocks/page:render :around ((app app) inner-html &key dependencies)
;;   (declare (ignore dependencies))
;;   (time (call-next-method)))


(defmethod get-dependencies ((app app))
  (list*
   ;; (reblocks-lass:make-dependency
   ;;   '(body
   ;;     :background "rgb(51, 53, 65)"
   ;;     :color "rgb(235, 236, 241)"))
   (call-next-method)))


(defmethod render-headers ((app app))
  (with-html
    (:link :rel "shortcut icon"
           :href "/favicon.ico"
           :type "image/x-icon")

    (:meta :property "og:type"
           :content "article")
    (:meta :name "twitter:card"
           :content "summary_large_image")
    
    (:meta :property "og:url"
           :content (reblocks/request:get-uri))
    
    (:meta :property "og:image"
           :content "https://chit-chat.ru/images/link-preview.png")
    ;; Размеры в два раза меньше реальных
    (:meta :property "og:image:width"
           :content "600")
    (:meta :property "og:image:height"
           :content "315")
    
    (:meta :name "twitter:image"
           :content "https://chit-chat.ru/images/link-preview.png")
    
    (:meta :property "og:title"
           :content (or (reblocks/page:get-title)
                        "Обсуждайте в прямом эфире!"))
    (:meta :name "twitter:title"
           :content (or (reblocks/page:get-title)
                        "Обсуждайте в прямом эфире!"))
    
    ;; Если добавить дескрипшн, то картинка превратится в мелкую иконку:
    ;; (:meta :property "og:description"
    ;;        :content "Можно ещё какой-нибудь description зафигачить.")
    (:meta :property "og:site_name"
           :content "Chit-Chat.ru"))
  
  (call-next-method))
