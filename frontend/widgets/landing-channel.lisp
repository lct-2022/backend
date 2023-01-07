(uiop:define-package #:app/widgets/landing-channel
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks-websocket
                #:websocket-widget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:app/program
                #:min-left
                #:programme-title
                #:channel-image-url
                #:channel-name)
  (:import-from #:app/controllers/programme-workflow
                #:programme-workflow
                #:programme
                #:latest-chat-message
                #:channel
                #:chat-id)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-lass)
  (:import-from #:app/vars
                #:*light-background*)
  (:import-from #:app/bus
                #:add-event-handler
                #:make-event-handler)
  (:import-from #:app/utils/user
                #:get-user-name
                #:get-user-avatar))
(in-package #:app/widgets/landing-channel)


(defwidget landing-channel-widget (websocket-widget)
  ((workflow :initarg :workflow
             :accessor workflow))
  (:documentation "Этот виджет подписывается на события и обновляется в следующих случаях:

- (:new-minute) - чтобы обновилось время до окончания программы;
- (:program-was-started workflow) - чтобы заменить программу в блоке.
- (:new-message message) - чтобы обновить поледнее сообщение."))



(defun update-ignoring-missing-websockets (widget)
  (handler-bind ((reblocks-websocket:no-active-websockets #'continue))
    (reblocks/widget:update widget)))

(declaim (notinline on-new-minute-callback))
(defun on-new-minute-callback (widget)
  (update-ignoring-missing-websockets widget))


(declaim (notinline on-new-program-callback))
(defun on-new-program-callback (widget workflow)
  (let* ((old-workflow (workflow widget))
         (old-channel (channel old-workflow))
         (new-channel (channel workflow))
         (new-programme (programme workflow)))
    (cond
      ((mito:object= old-channel new-channel)
       (ignore-errors
        (log4cl-extras/error:with-log-unhandled ()
          (log:debug "Switching to new program" new-programme)))

       (setf (workflow widget)
             workflow)
       (update-ignoring-missing-websockets widget))
      (t
       ;; (log:debug "Ignoring" new-programme "because it is for" new-channel "but this widget is for" old-channel)
       ))))


(declaim (notinline on-new-message-callback))
(defun on-new-message-callback (widget message)
  (ignore-errors
   (log4cl-extras/error:with-log-unhandled ()
     (let* ((message-from-chat-id (chat/client:message-chat-id message))
            (workflow (workflow widget))
            (widget-chat-id (chat-id workflow)))
       (when (equal widget-chat-id
                    message-from-chat-id)
         (log:debug "Processing new message" message)

         (let* ((text (chat/client:message-message message))
                (user-id (chat/client:message-user-id message))
                (nickname (get-user-name user-id))
                (avatar-url (get-user-avatar user-id)))
           (setf (latest-chat-message workflow)
                 (list text nickname avatar-url))
           (update-ignoring-missing-websockets widget)))))))


(defun make-landing-channel-widget (workflow)
  (check-type workflow programme-workflow)
  (let* ((widget (make-instance 'landing-channel-widget
                                :workflow workflow)))
    (log:debug "Initializing new landing channel widget")
    
    (add-event-handler :new-minute
        (make-event-handler on-new-minute ()
          (on-new-minute-callback widget)))
    
    (add-event-handler :program-was-started
        (make-event-handler on-new-program  (workflow)
          (on-new-program-callback widget workflow)))
    
    (add-event-handler :new-message
        (make-event-handler on-new-message  (message)
          (on-new-message-callback widget message)))
    
    widget))


(defmethod render ((widget landing-channel-widget))
  (let* ((workflow (workflow widget))
         (channel (channel workflow))
         (programme (programme workflow))
         (ch-title (channel-name channel))
         (ch-image-url (channel-image-url channel))
         (pr-title (programme-title programme)))
    (flet ((open-chat (&rest args)
             (declare (ignore args))
             (let* ((chat-id (chat-id workflow)))
               (redirect (fmt "/chat/~A" chat-id)))))
      (destructuring-bind (&optional last-message nickname avatar-url)
          (latest-chat-message workflow)
        (with-html
          (:div :class "chat"
                :onclick (make-js-action #'open-chat)
                (:div :class "chat-content"
                      (:div :class "channel-title"
                            (when ch-image-url
                              (:img :class "channel-logo"
                                    :src ch-image-url))
                            ch-title)
                      (:div :class "program-title"
                            :title pr-title
                            pr-title)
                      (:div :class "program-progress"
                            (:div :class "left"
                                  (if (zerop (min-left programme))
                                      "заканчивается"
                                      (fmt "осталось ~A мин" (min-left programme)))))
                      
                      (when last-message
                        (:div :class "last-message"
                              (:img :class "nickname"
                                    :src avatar-url
                                    :title nickname)
                              (:div :class "text"
                                    last-message))))))))))


(defmethod get-dependencies ((widget landing-channel-widget))
  (list* (reblocks-lass:make-dependency
           `(.landing-channel-widget
             :cursor pointer
             :background-color ,*light-background*
             :min-width "30%"
             :max-width "30%"
             :padding 1em
             :padding-bottom 0.6rem
             :border-radius 0.5em
             :font-size 1.4em
             :font-weight 500
             
             (.chat-content
              ((:or .program-title
                    .channel-title)
               (a :color "rgb(235, 236, 241)") 
               :overflow hidden
               :white-space nowrap
               :text-overflow ellipsis)
              (.last-message
               :font-size 1rem
               :display flex
               :margin-top 0.5rem
               (.nickname
                :width 1.3rem
                :height 1.3rem
                :margin-right 0.2rem)
               ;; ((:and .text :before)
               ;;  :content ":")
               (.text
                :overflow hidden
                :white-space nowrap
                :text-overflow ellipsis))
              (.channel-title
               (.channel-logo
                :width 32px
                :height 32px
                :margin-right 0.5rem))
              (.program-progress
               :display flex
               (.done
                :background white
                :height 2px
                :margin-top 0.7rem)
               (.left
                :white-space nowrap
                :font-size 0.8rem
                :margin-right 0.5rem)))))

         (reblocks-lass:make-dependency
           `(:media "(max-width: 600px)"
                    (.landing-channel-widget
                     :min-width 80%
                     :max-width 100%)))
         (call-next-method)))
