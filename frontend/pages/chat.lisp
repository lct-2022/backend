(uiop:define-package #:app/pages/chat
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:get-css-classes
                #:widget
                #:render
                #:defwidget)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:push-end
                #:fmt)
  (:import-from #:app/vars
                #:*url-prefix*)
  (:import-from #:passport/client
                #:make-passport)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:app/forms
                #:with-html-form)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:chat/client
                #:make-chat-api)
  (:import-from #:event-emitter
                #:emit
                #:event-emitter)
  (:import-from #:alexandria
                #:lastcar
                #:appendf)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:make-chat-page))
(in-package #:app/pages/chat)



(defwidget message-widget ()
  ((message :initarg :message
            :accessor message)))


(defwidget post-form-widget (event-emitter widget)
  ((chat-id :initarg :chat-id
            :accessor chat-id)))


(defwidget chat-page ()
  ((chat-id :initform nil
            :accessor chat-id)
   (messages :initform nil
             :accessor messages)
   (post-form :initarg :post-form
              :accessor post-form)))


(defun make-chat-page ()
  (let* ((form (make-instance 'post-form-widget))
         (chat-page (make-instance 'chat-page
                                   :post-form form)))
    (flet ((add-new-message-to-the-list (message)
             (log:info "Adding message to the list" message)
             (let ((widget (make-message-widget message))
                   (prev-message-widget (lastcar (messages chat-page))))
               (push-end widget
                         (messages chat-page))
               (reblocks/widget:update widget :inserted-after prev-message-widget))))
      (event-emitter:on :new-message-posted form
                        #'add-new-message-to-the-list)
      (values chat-page))))


(defmethod initialize-instance :after chat-page)


(defun make-message-widget (message)
  (make-instance 'message-widget
                 :message message))


(defun fetch-recent-messages (chat-id)
  (let ((api (chat/client::connect
              (make-chat-api)
              (get-user-token))))
    (mapcar #'make-message-widget
            (chat/client:get-messages api chat-id))))


(defcached (get-current-user-profile :timeout 15) ()
  (let* ((api (passport/client::connect
               (make-passport)
               (get-user-token))))
    (passport/client:my-profile api)))


(defcached (get-user-profile :timeout 15) (user-id)
  (let* ((api (passport/client::connect
               (make-passport)
               (get-user-token))))
    (passport/client:get-profile api user-id)))


(defun get-current-user-id ()
  (handler-case
      (passport/client:user-id
       (get-current-user-profile))
    (openrpc-client/error:rpc-error ()
      nil)))


(defun get-current-user-avatar ()
  (passport/client:user-avatar-url
   (get-current-user-profile)))

(defun get-user-avatar (user-id)
  (passport/client:user-avatar-url
   (get-user-profile user-id)))





(defmethod render ((widget chat-page))
  (setf *widget* widget)

  (register-groups-bind (current-chat-id)
      ("^/chat/(.*)$" (get-path))
    (unless (string-equal current-chat-id
                          (chat-id widget))
      (setf (chat-id (post-form widget)) current-chat-id
            (chat-id widget) current-chat-id
            (messages widget)
            (fetch-recent-messages current-chat-id))))
  
  (with-html
    (cond
      ((messages widget)
       (:div :class "messages"
             (mapc #'render (messages widget))))
      (t
       (:p "В этом чате пока нет сообщений. Стань первым!")))
    (render (post-form widget))))


(defmethod render ((widget message-widget))
  (with-html
    (let* ((msg (message widget))
           (author-id (chat/client:message-user-id msg))
           (current-user-id (get-current-user-id))
           (avatar-url (get-user-avatar author-id)))
      
      (:img :class "message-avatar"
            :src avatar-url)
      (:div :class "message-text"
            (chat/client:message-message msg)))))


(defmethod get-css-classes ((widget message-widget))
  (append (call-next-method)
          (when (get-user-token)
            (let* ((msg (message widget))
                   (author-id (chat/client:message-user-id msg))
                   (current-user-id (get-current-user-id)))
              (when (= author-id current-user-id)
                (list "from-current-user"))))))


(defvar *widget* nil)

(defmethod render ((widget post-form-widget))
  (flet ((post-message (&key message &allow-other-keys)
           (let* ((api (chat/client::connect
                        (make-chat-api)
                        (get-user-token)))
                  (message (chat/client:post api (chat-id widget) message)))
             (emit :new-message-posted widget message))))
    (cond
      ((get-user-token)
       (with-html-form (:post #'post-message)
         (:textarea :name :message
                    :placeholder "Сюда надо что-то написать."
                    :rows 5)
         (:input :type "submit"
                 :class "button success"
                 :value "Отправить")))
      (t
       (with-html
         (:p ("Чтобы что-то написать, надо [залогиниться](/login).")))))))


(defmethod get-dependencies ((widget chat-page))
  (list
   (reblocks-lass:make-dependency
     '(.chat-page
       :width 50%
       :margin-left auto
       :margin-right auto
       :margin-top 2rem
       :display flex
       :flex-direction column
       :gap 2rem
       (.messages
        :display flex
        :flex-direction column
        :gap 1rem
        (.message-widget
         :display flex
         :flex-direction row
         :gap 1rem
         (.message-avatar
          :width 3rem
          :height 3rem)
         (.message-text
          :background white
          :padding 0.5rem
          :border-radius 0.5rem))
        ((:and .message-widget .from-current-user)
         :flex-direction row-reverse))))))


(defmethod get-dependencies ((widget post-form-widget))
  (list
   (reblocks-lass:make-dependency
     '(.post-form-widget
       (textarea :border-radius 0.5rem)
       ((> form)
        :display flex
        :flex-direction column)))))
