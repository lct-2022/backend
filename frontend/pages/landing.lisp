(uiop:define-package #:app/pages/landing
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/utils
                #:format-time
                #:get-user-token)
  (:import-from #:app/program
                #:get-channels-for-landing
                #:programme-chat-id
                #:programme-channel-id
                #:programme-start
                #:programme-stop
                #:channel-image-url
                #:channel-name
                #:programme-title
                #:get-programs-for-landing
                #:program-end
                #:program-title
                #:program
                #:program-begin)
  (:import-from #:local-time
                #:timestamp<
                #:format-timestring
                #:+iso-8601-time-format+
                #:now)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-lass)
  (:import-from #:chat/client
                #:make-chat-api)
  (:import-from #:local-time-duration
                #:duration
                #:timestamp-duration-)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:serapeum
                #:random-in-range
                #:fmt)
  (:import-from #:app/vars
                #:*text-color*
                #:*light-background*)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:chat/message/model)
  (:import-from #:sxql)
  (:import-from #:passport/user)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:passport/client
                #:make-passport)
  (:import-from #:app/controllers/programme-workflow
                #:channel
                #:archived-p
                #:get-workflows-for-channels)
  (:import-from #:app/widgets/landing-channel
                #:workflow
                #:make-landing-channel-widget))
(in-package #:app/pages/landing)


(defvar *program-to-chat*
  (make-hash-table :test 'equal))


(defwidget landing-page ()
  ((widgets :initform nil
            :accessor widgets)))


(defun make-landing-page ()
  (make-instance 'landing-page))


(defcached (get-user-nickname :timeout 60) (user-id)
  ;; TODO: позже надо будет сделать в API чатов ручку
  (with-connection ()
    (let* ((user (mito:find-dao 'passport/user::user
                                :id user-id)))
      (when user
        (passport/user::user-nickname user)))))


(defcached (get-user-avatar :timeout 60) (user-id)
  ;; TODO: позже надо будет сделать в API чатов ручку
  (let* ((api (passport/client::connect
               (make-passport)
               (get-user-token)))
         (profile (passport/client::get-profile api user-id))
         (avatar-url (when profile
                       (passport/client::user-avatar-url profile))))
    avatar-url))


(defun get-latest-chat-message (programme)
  (let* ((chat-id (get-chat-for-program programme)))
    (when chat-id
      ;; TODO: позже надо будет сделать в API чатов ручку
      (with-connection ()
        (let* ((message (first
                         (mito:select-dao 'chat/message/model::message
                           (sxql:where (:= :chat-id chat-id))
                           (sxql:order-by (:desc :created-at))
                           (sxql:limit 1))))
               (user-id (chat/message/model::user-id message)))
          (values (chat/message/model::chat-message message)
                  (get-user-nickname user-id)
                  (get-user-avatar user-id)))))))


(defun init-widgets-if-needed (landing-page)
  (let ((widgets (widgets landing-page)))
    ;; В некоторых виджетах могут остаться передачи, которые уже закончились.
    ;; их надо обновить:
    (cond
      (widgets
       (loop for widget in widgets
             for workflow = (workflow widget)
             for channel = (channel workflow)
             when (archived-p workflow)
             do (let ((new-workflow (first (get-workflows-for-channels (list channel)))))
                  (if new-workflow
                      (setf (workflow widget)
                            new-workflow)
                      (log:warn "Unable to find workflow for" channel)))))
      (t
       (let* ((channels (get-channels-for-landing))
              (workflows (get-workflows-for-channels channels))
              (widgets (mapcar #'make-landing-channel-widget workflows)))
         (setf (widgets landing-page)
               widgets)))))
  (values))


(defmethod render ((widget landing-page))
  (with-simple-restart (ignore "Continue without subwidgets update")
    (init-widgets-if-needed widget))
  
  (with-html
    (:h2 :class "motto"
         "Обсуждайте телепередачи с теми, кто тоже их смотрит!")
    
    (:div :class "chats"
          (mapc #'render (widgets widget))

          (:div :class "chats-link"
                (:div :class "channel-title"
                      (:a :href "/channels/"
                          "Все каналы"))))))


(defun get-programme-chat-by-id (chat-id)
  (with-connection ()
    (mito:find-dao 'app/program::programme-chat
                   :chat-id chat-id)))


;; (defun get-chat-for-program (programme)
;;   (with-connection ()
;;     (let ((chat (mito:find-dao 'app/program::programme-chat
;;                                :channel-id (programme-channel-id programme)
;;                                :start (programme-start programme))))
;;       (when chat
;;         (programme-chat-id chat)))))


(defun create-chat-for-program (programme)
  (error "Delete me")
  ;; (let ((program-title (programme-title programme)))
  ;;   (or (get-chat-for-program programme)
  ;;       (progn
  ;;         (log:info "Creating chat for" program-title)
    
  ;;         (let* ((client (chat/client::connect (make-chat-api)))
  ;;                (chat (chat/client:create-chat client
  ;;                                               :title program-title))
  ;;                (chat-id (chat/client:chat-id chat)))
  ;;           (chat/client:create-fake-messages
  ;;            client
  ;;            chat-id
  ;;            (random-in-range 3 10))

  ;;           (with-connection ()
  ;;             (mito:create-dao 'app/program::programme-chat
  ;;                              :channel-id (programme-channel-id programme)
  ;;                              :start (programme-start programme)
  ;;                              :stop (programme-stop programme)
  ;;                              :title program-title
  ;;                              :chat-id chat-id))
  ;;           (values chat-id)))))
  )


(defmethod get-dependencies ((widget landing-page))
  (list* (reblocks-lass:make-dependency
           `(.landing-page
             (.motto
              :font-size 1.3rem
              :text-align center
              :margin-left -112px)
             (.chats
              :display flex
              :flex-direction row
              :flex-wrap wrap
              :gap 1em
              :margin-top 2em
              :margin-bottom 4em
              ;; (.chat
              ;;  :cursor pointer
              ;;  :background-color ,*light-background*
              ;;  :min-width "30%"
              ;;  :max-width "30%"
              ;;  :padding 1em
              ;;  :padding-bottom 0.6rem
              ;;  :border-radius 0.5em
              ;;  :font-size 1.4em
              ;;  :font-weight 500
              ;;  (.chat-content
              ;;   ((:or .program-title
              ;;         .channel-title)
              ;;    (a :color "rgb(235, 236, 241)") 
              ;;    :overflow hidden
              ;;    :white-space nowrap
              ;;    :text-overflow ellipsis)
              ;;   (.last-message
              ;;    :font-size 1rem
              ;;    :display flex
              ;;    :margin-top 0.5rem
              ;;    (.nickname
              ;;     :width 1.3rem
              ;;     :height 1.3rem
              ;;     :margin-right 0.2rem)
              ;;    ;; ((:and .text :before)
              ;;    ;;  :content ":")
              ;;    (.text
              ;;     :overflow hidden
              ;;     :white-space nowrap
              ;;     :text-overflow ellipsis))
              ;;   (.channel-title
              ;;    (.channel-logo
              ;;     :width 32px
              ;;     :height 32px
              ;;     :margin-right 0.5rem))
              ;;   (.program-progress
              ;;    :display flex
              ;;    (.done
              ;;     :background white
              ;;     :height 2px
              ;;     :margin-top 0.7rem)
              ;;    (.left
              ;;     :white-space nowrap
              ;;     :font-size 0.8rem
              ;;     :margin-right 0.5rem))))

              (.chats-link
               :min-width "30%"
               :padding 1em
               :font-size 2.2rem
               :text-align center
               (a :color ,*text-color*)))
             (.program
              :display flex
              :flex-flow column
              :gap 0.5rem
              (.program-item
               :display flex            
               :gap 1rem
               (input :margin 0)))))

         (reblocks-lass:make-dependency
           `(:media "(max-width: 600px)"
                    (.landing-page
                     (.chats
                      :flex-direction column
                      :margin-top 1rem
                      :margin-bottom 1rem
                      ;; (.chat
                      ;;  :min-width 80%
                      ;;  :max-width 100%)
                      ))))
         (call-next-method)))
