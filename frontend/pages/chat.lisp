(uiop:define-package #:app/pages/chat
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:get-css-classes
                #:widget
                #:render
                #:defwidget)
  (:import-from #:parenscript)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:length>
                #:push-end
                #:fmt)
  (:import-from #:app/vars
                #:*text-color*
                #:*light-background*
                #:*dark-background*
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
                #:length=
                #:lastcar
                #:appendf)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:local-time
                #:parse-timestring)
  (:import-from #:local-time-duration
                #:duration-as
                #:timestamp-difference)
  (:import-from #:humanize-duration
                #:humanize-duration)
  (:import-from #:bordeaux-threads
                #:make-recursive-lock)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  (:import-from #:3bmd
                #:parse-string-and-print-to-stream)
  (:import-from #:str
                #:join
                #:replace-all)
  (:import-from #:cl-emoji
                #:with-emoji-list)
  (:import-from #:reblocks/response
                #:send-script)
  (:import-from #:reblocks/cached-dependencies-mixin
                #:cached-dependencies-mixin)
  (:import-from #:app/pages/landing
                #:get-programme-chat-by-id)
  (:import-from #:app/program
                #:channel-image-url
                #:channel-name
                #:get-channel-by-id
                #:programme-channel-id)
  (:export
   #:make-chat-page))
(in-package #:app/pages/chat)



(defwidget message-widget ()
  ((message :initarg :message
            :accessor message)))


(defwidget post-form-widget (event-emitter cached-dependencies-mixin widget)
  ((chat-id :initarg :chat-id
            :accessor chat-id)))


(defun make-known-ids-hash ()
  (make-hash-table :synchronized t))


(defwidget chat-page ()
  ((chat-id :initform nil
            :accessor chat-id)
   (title :initform nil
          :accessor chat-title)
   (messages :initform nil
             :accessor messages)
   (error-message :initform nil
                  :accessor error-message)
   (known-ids :initform (make-known-ids-hash)
              :accessor known-ids)
   (add-messages-lock :initform (make-recursive-lock "add-chat-messages")
                      :reader add-messages-lock)
   (next-page-key :initform nil
                  :accessor next-page-key)
   (post-form :initarg :post-form
              :accessor post-form)))


(defun scroll-to (widget &key (smooth t) (focus-on-form nil))
  (reblocks/response:send-script
   (ps:ps* `(progn
              ;; (ps:chain console
              ;;           (log "Scrolling to element " ,(dom-id widget)))
              (let ((element (ps:chain document
                                       (get-element-by-id ,(dom-id widget)))))
                (ps:chain element
                          (scroll-into-view
                           (ps:create "behavior" ,(if smooth
                                                      "smooth"
                                                      "auto")
                                      "block" "center")))
                (when ,focus-on-form
                  (on-scroll-end
                   window
                   (lambda ()
                     (focus-on-form-textarea))
                   500)))))))


(defun make-chat-page ()
  (let* ((form (make-instance 'post-form-widget))
         (chat-page (make-instance 'chat-page
                                   :post-form form)))
    (flet ((add-new-message-to-the-list (message)
             (log:info "Adding message to the list" message)
             (bt:with-lock-held ((add-messages-lock chat-page))
               (let ((widget (make-message-widget message))
                     (prev-message-widget (lastcar (messages chat-page))))
                 (setf (gethash (chat/client:message-id message)
                                (known-ids chat-page))
                       t)
                 (push-end widget
                           (messages chat-page))
                 (reblocks/widget:update widget :inserted-after prev-message-widget)
                 (scroll-to widget :focus-on-form t)))))
      (event-emitter:on :new-message-posted form
                        #'add-new-message-to-the-list)
      (values chat-page))))


(defun make-message-widget (message)
  (make-instance 'message-widget
                 :message message))


(defun fetch-new-messages (widget &key insert-to-dom)
  ;; (let ((page-key (next-page-key widget)))
  ;;   (log:info "Checking next messages with key" page-key))
  
  (bt:with-lock-held ((add-messages-lock widget))
    (let* ((chat-id (chat-id widget))
           (api (chat/client::connect
                 (make-chat-api)
                 (get-user-token)))
           (retrieve-first-page-func (lambda ()
                                       (chat/client:get-messages api chat-id :page-key (next-page-key widget))))
           (messages
             (loop for retrieve-func = retrieve-first-page-func then retrieve-next-page-func
                   while retrieve-func
                   for (messages-chunk retrieve-next-page-func) = (multiple-value-list
                                                                   (funcall retrieve-func))
                   append messages-chunk))
           (last-msg (lastcar messages))
           (prev-message-widget (lastcar (messages widget)))
           (next-page-key (when last-msg
                            (chat/client:message-id last-msg)))
           (new-widgets
             (loop for message in messages
                   for message-id = (chat/client:message-id message)
                   unless (gethash message-id (known-ids widget))
                     do (setf (gethash message-id (known-ids widget))
                              t)
                     and
                       collect (make-message-widget message))))
      ;; Сделаем так, чтобы новые сообщения появились на странице
      (when insert-to-dom
        (loop for message-widget in new-widgets
              do (log:info "Inserting message after" prev-message-widget)
                 (reblocks/widget:update message-widget :inserted-after prev-message-widget)
                 (setf prev-message-widget message-widget)))

      ;; Добавим их в кэш
      (appendf (messages widget)
               new-widgets)

      ;; Прокрутим окно чата:
      (when new-widgets
        (scroll-to (lastcar new-widgets)))
      
      ;; Если подтянули новые сообщения, то сдвинем указатель, чтобы при следующих обновлениях получить только новые сообщения
      (when next-page-key
        (setf (next-page-key widget)
              next-page-key))
      (values))))


(defcached (%get-current-user-profile :timeout 15) (token)
  (let* ((api (passport/client::connect
               (make-passport)
               token)))
    (passport/client:my-profile api)))


(defun get-current-user-profile ()
  (let ((token (get-user-token)))
    (when token
      (%get-current-user-profile token))))


(defcached (get-user-profile :timeout 15) (user-id)
  (let* ((api (passport/client::connect
               (make-passport)
               (get-user-token))))
    (passport/client:get-profile api user-id)))


(defun get-current-user-id ()
  (let ((profile (get-current-user-profile)))
    (when profile
      (handler-case
          (passport/client:user-id profile)
        (openrpc-client/error:rpc-error ()
          nil)))))


(defun get-current-user-avatar ()
  (passport/client:user-avatar-url
   (get-current-user-profile)))

(defun get-user-avatar (user-id)
  (passport/client:user-avatar-url
   (get-user-profile user-id)))

(defun get-user-name (user-id)
  (passport/client:user-nickname
   (get-user-profile user-id)))


(defmethod render ((widget chat-page))
  (register-groups-bind (current-chat-id)
      ("^/chat/(.*)$" (get-path))
    (unless (string-equal current-chat-id
                          (chat-id widget))
      ;; Сначала убедимся, что такой чат есть
      (let* ((api (chat/client::connect
                   (make-chat-api)
                   (get-user-token))))

        (handler-case
            (let ((chat (chat/client:get-chat api current-chat-id)))
              (setf (chat-id (post-form widget)) current-chat-id
                    (chat-id widget) current-chat-id
                    (chat-title widget) (chat/client:chat-title chat)
                    (error-message widget) nil
                    (messages widget) nil
                    (known-ids widget) (make-known-ids-hash))
              (fetch-new-messages widget))
          (openrpc-client/error:rpc-error (e)
            (setf (error-message widget)
                  (openrpc-client/error:rpc-error-message e)))))))

  (with-html
    (cond
      ((error-message widget)
       (:p :class "error"
           (error-message widget)))
      (t
       (flet ((retrieve-messages (&key &allow-other-keys)
                (fetch-new-messages widget :insert-to-dom t)))
         (let* ((chat-id (chat-id widget))
                (programme-chat (get-programme-chat-by-id chat-id))
                (channel (get-channel-by-id (programme-channel-id programme-chat)))
                (channel-title (channel-name channel))
                (channel-logo-url (channel-image-url channel))
                (action-code (reblocks/actions:make-action #'retrieve-messages))
                ;; TODO: позже надо будет прикрутить отправку новых сообщений через websocket или server-side-events
                (action (ps:ps* `(set-interval
                                  (lambda ()
                                    ;; (ps:chain console
                                    ;;           (log "Fetching fresh messages"))
                                    (initiate-action ,action-code)
                                    nil)
                                  3000))
                        ;; (ps:ps* `(defun fetch-messages ()
                        ;;            (ps:chain console
                        ;;                      (log "Fetching fresh messages"))
                        ;;            (initiate-action ,action-code)
                        ;;            nil))
                        ))
           (:script (:raw action))
           (when (chat-title widget)

             (setf (reblocks/page:get-title)
                   (serapeum:fmt "~A - ~A"
                                 channel-title
                                 (chat-title widget)))
             
             (:div :class "chat-header"
                   ;; TODO: надо чат к каналу как-то привязывать и к программе
                   ;; (:h1 :class "channel-title"
                   ;;      (:span :class "icon"
                   ;;             (:raw "<svg width=\"28\" height=\"38\" viewBox=\"0 0 28 36\" fill=\"none\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"M0 13.57l1.307-4.616L28 0l-8.773 31.385-10.08 2.769 7.28-26.123L0 13.569zM2.427 36l6.626-24 5.227-1.754-6.813 24.37L2.427 36z\" fill=\"#55C\"></path></svg>"))
                   ;;      (:span  "Первый канал"))
                   (:h2 :class "channel-title"
                        (when channel-logo-url
                          (:img :class "channel-logo"
                                :src channel-logo-url
                                :title (format nil "Логотип канала ~A"
                                               channel-title)))

                        channel-title)
                   (:h2 :class "chat-title"
                        (chat-title widget))
                   (:p "Осталось 12 минут.")))
           (:div :class "messages"
                 (cond
                   ((messages widget)
                    (mapc #'render (messages widget))
                    (scroll-to (lastcar (messages widget))
                               :smooth nil))
                   (t
                    (:p :class "error-message"
                        "В этом чате пока нет сообщений. Стань первым!"))))
           (render (post-form widget))))))))


(defvar *caret-return* (coerce (list #\Return) 'string))


(defun render-emoji (text)
  (if (and (length> text 2)
           (char-equal (elt text 0)
                       #\:)
           (char-equal (elt text (1- (length text)))
                       #\:))
      (or (cl-emoji:alpha-code text)
          text)
      text))


(defvar *all-emoji*
           (with-emoji-list (el)
             (reduce (lambda (s e) (concatenate 'string s (getf e :characters)))
                     el :initial-value "")))


(defun only-one-emoji (text)
  "Возвращает True если строка состоит из одного единственного Emoji символа."
  (and (length= 1 text)
       (find (elt text 0) *all-emoji*)))


(defun render-message-text (text)
  (let* ((without-caret-return (replace-all *caret-return* "" text))
         (trimmed (str:trim without-caret-return))
         (maybe-with-emoji (render-emoji trimmed))
         (html (with-output-to-string (s)
                 (parse-string-and-print-to-stream maybe-with-emoji
                                                   s))))
    (values html
            (only-one-emoji maybe-with-emoji))))


(defmethod render ((widget message-widget))
  (with-html
    (let* ((msg (message widget))
           (author-id (chat/client:message-user-id msg))
           (created-at (parse-timestring (chat/client:message-created-at msg)))
           (since (timestamp-difference (local-time:now)
                                        created-at))
           (since-as-str (if (zerop (duration-as since :sec))
                             "только что"
                             (humanize-duration since
                                                :n-parts 1
                                                :format-part #'humanize-duration/ru:format-part)))
           (avatar-url (get-user-avatar author-id))
           (author-name (get-user-name author-id)))
      (multiple-value-bind (processed-message one-emoji)
          (render-message-text (chat/client:message-message msg))

        (let ((classes (append (list "message-text")
                               (when one-emoji
                                 (list "only-one-emoji")))))
          (:div :class "message-author"
                (:img :class "message-avatar"
                      :src avatar-url
                      :title author-name)
                ;; (:span :class "author-name"
                ;;        )
                )
          (:div :class "message-body"
                (:div :class (join " " classes)
                      (:raw processed-message))
                ;; (:div :class "message-time"
                ;;       since-as-str)
                ))))))


(defmethod get-css-classes ((widget message-widget))
  (append (call-next-method)
          (when (get-user-token)
            (let* ((msg (message widget))
                   (author-id (chat/client:message-user-id msg))
                   (current-user-id (get-current-user-id)))
              (when (= author-id current-user-id)
                (list "from-current-user"))))))


(defmethod reblocks/widget:update :after ((widgete post-form-widget) &key &allow-other-keys)
  (send-script (ps:ps (initialize-form))
               t))

(defmethod render ((widget post-form-widget))
  (flet ((post-message (&key message &allow-other-keys)
           (setf message
                 (str:trim message))
           (unless (string-equal message "")
             (log:info "Posting" message)

             (let* ((api (chat/client::connect
                          (make-chat-api)
                          (get-user-token)))
                    (message (chat/client:post api (chat-id widget) message)))
               ;; Сбросим состояние окна для ввода сообщения
               (reblocks/widget:update widget)
               (emit :new-message-posted widget message)))))
    (cond
      ((get-user-token)
       (with-html-form (:post #'post-message)
         (:textarea :name :message
                    :placeholder "Сюда надо что-то написать."
                    :rows 2)
         (:input :type "submit"
                 :class "send-button"
                 :value "")))
      (t
       (with-html
         (:p ("Чтобы что-то написать, надо [залогиниться](/login).")))))))


(defmethod get-dependencies ((widget chat-page))
  (list*
   (reblocks-lass:make-dependency
     `(.chat-page
       :margin-left auto
       :margin-right auto
       :margin-top 2rem
       :display flex
       :flex-direction column
       :gap 2rem
       (.chat-header
        :width 60%
        :margin-left auto
        :margin-right auto)
       (.channel-title
        :font-size 2.2rem
        (.channel-logo
         :height 2.2rem
         :margin-right 0.5rem
         :position relative
         :margin-top -0.5rem))
       (.chat-title
        :font-size 2.2em)
       (.messages
        :display flex
        :flex-direction column
        :padding-bottom 100px
        (.error-message
         :width 60%
         :margin-left auto
         :margin-right auto)
        (.message-widget
         :display flex
         :flex-direction row
         :gap 1rem
         :padding-left 20%
         :padding-right 20%
         :padding-top 2rem
         :padding-bottom 2rem
         :background-color ,*light-background*
         :border-top 2px solid ,*dark-background*
         (.message-author
          :display flex
          :max-width 64px
          :min-width 64px
          :height 64px
          :flex-direction column)
         (.message-avatar
          :border-radius 1.5rem)
         (.message-body
          :display flex
          :flex-direction column
          :align-items flex-end
          (.message-time
           :font-size 0.7rem
           :color gray)
          (.message-text
           :padding 0.5rem
           :border-radius 0.5rem)
          ((:and .message-text .only-one-emoji)
           :font-size 10rem)))
        ((:and .message-widget .from-current-user)
         :border-top 2px solid ,*light-background*
         :background-color ,*dark-background*))))
   
   (reblocks-lass:make-dependency
     `(:media "(max-width: 600px)"
              (.chat-page
               :margin-top 1rem
               (.chat-header
                :margin-left 0
                :margin-right 0
                :width 100%
                :text-align center
                (.chat-title
                 :font-size 1.2rem
                 :white-space nowrap
                 :overflow hidden
                 :text-overflow ellipsis
                 :font-weight bold))
               (.messages
                (.message-widget
                 :padding-left 1rem
                 :padding-right 1rem
                 :padding-top 0
                 :padding-bottom 0.5rem)))))

   (call-next-method)))


(defmethod get-dependencies ((widget post-form-widget))
  (list*
   (let ((form-id (format nil "#~A form" (dom-id widget)))
         (textarea-id (format nil "#~A textarea" (dom-id widget))))
     (reblocks-parenscript:make-dependency*
      `(progn
         (defun focus-on-form-textarea ()
           (ps:chain (j-query ,textarea-id)
                     (focus)))
         
         (defun initialize-form ()
           (let ((form (j-query ,form-id)))
             (unless (ps:chain form
                               (data "submit-initialized"))
               (ps:chain form
                         (keydown (lambda (event)
                                    (when (= (ps:@ event key-code)
                                             13)
                                      ;; (ps:chain console
                                      ;;           (log "Enter was pressed in the form" event))

                                      (ps:chain (j-query this)
                                                (trigger "submit"))
                                      nil)
                                    nil)))
               (ps:chain form
                         (data "submit-initialized" t))))
           (focus-on-form-textarea)
           nil)

         (defun on-scroll-end (obj callback timeout)
           (let (($this (j-query obj)))
             (ps:chain $this
                       (on "scroll"
                           (lambda ()
                             ;; (ps:chain console
                             ;;           (log "Still scrolling"))
                             (let ((timeout (ps:chain $this
                                                      (data "scrollTimeout"))))
                               (when timeout
                                 (clear-timeout timeout))
                               (ps:chain $this
                                         (data "scrollTimeout"
                                               (set-timeout callback timeout)))))))))

         ;; Инициализируем форму после первоначальной загрузки страницы
         (ps:chain document
                   (add-event-listener
                    "DOMContentLoaded"
                    initialize-form)))))

   (reblocks-lass:make-dependency
     `(.post-form-widget
       :width 100%
       ((> form)
        :position fixed
        :bottom 0
        :width 60%
        :left 50%
        :transform "translate(-50%, -50%)"

        :display flex
        :flex-direction row
        :margin-left auto
        :margin-right auto
        (textarea
         :border-radius 0.5rem
         :margin-bottom -1rem
         :background ,*light-background*
         :color ,*text-color*)

        (.send-button
         :border 0
         :position relative
         :top 2rem
         :left 3rem
         :transform "translate(-50%, -50%)"
         :border-radius 50%
         :min-width 60px
         :min-height 60px
         :max-width 60px
         :max-height 60px
         :z-index 9999
         :background-color "#2CA5E0"
         :background-image "url(\"data:image/svg+xml;charset=UTF-8,%3csvg role='img' xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'%3e%3cpath fill='%23FFF' d='M23.91 3.79L20.3 20.84c-.25 1.21-.98 1.5-2 .94l-5.5-4.07-2.66 2.57c-.3.3-.55.56-1.1.56-.72 0-.6-.27-.84-.95L6.3 13.7l-5.45-1.7c-1.18-.35-1.19-1.16.26-1.75l21.26-8.2c.97-.43 1.9.24 1.53 1.73z'/%3e%3c/svg%3e\")"
         :background-size 50% 
         :background-repeat no-repeat
         :background-position 50% 50%))))
   
   (reblocks-lass:make-dependency
     `(:media "(max-width: 600px)"
              (.post-form-widget
               ((> form)
                :bottom -1rem
                :width 85%))))
   (call-next-method)))
