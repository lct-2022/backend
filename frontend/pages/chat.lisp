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
  (:export
   #:make-chat-page))
(in-package #:app/pages/chat)



(defwidget message-widget ()
  ((message :initarg :message
            :accessor message)))


(defwidget post-form-widget (event-emitter widget)
  ((chat-id :initarg :chat-id
            :accessor chat-id)))

(defun make-known-ids-hash ()
  (make-hash-table :synchronized t))


(defwidget chat-page ()
  ((chat-id :initform nil
            :accessor chat-id)
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


(defun scroll-to (widget &key (smooth t))
  (reblocks/response:send-script
   (ps:ps* `(progn
              (ps:chain console
                        (log "Scrolling to element " ,(dom-id widget)))
              (ps:chain document
                        (get-element-by-id ,(dom-id widget))
                        (scroll-into-view
                         (ps:create "behavior" ,(if smooth
                                                    "smooth"
                                                    "auto"))))))))


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
                 (scroll-to widget)))))
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


(defmethod render ((widget chat-page))
  (setf *widget* widget)

  (register-groups-bind (current-chat-id)
      ("^/chat/(.*)$" (get-path))
    (unless (string-equal current-chat-id
                          (chat-id widget))
      ;; Сначала убедимся, что такой чат есть
      (let* ((api (chat/client::connect
                   (make-chat-api)
                   (get-user-token))))

        (handler-case
            (progn (chat/client:get-chat api current-chat-id)
                   (setf (chat-id (post-form widget)) current-chat-id
                         (chat-id widget) current-chat-id
                         (error-message widget) nil
                         (messages widget) nil
                         (known-ids widget) (make-known-ids-hash))
                   (fetch-new-messages widget))
          (openrpc-client/error:rpc-error (e)
            (setf (error-message widget)
                  (openrpc-client/error:rpc-error-message e)))))))

  (cond
    ((error-message widget)
     (with-html
       (:p :class "error"
           (error-message widget))))
    (t
     (flet ((retrieve-messages (&key &allow-other-keys)
              (fetch-new-messages widget :insert-to-dom t)))
       (let* ((action-code (reblocks/actions:make-action #'retrieve-messages))
              ;; TODO: позже надо будет прикрутить отправку новых сообщений через websocket или server-side-events
              (action (ps:ps* `(set-interval
                                (lambda ()
                                  (ps:chain console
                                            (log "Fetching fresh messages"))
                                  (initiate-action ,action-code)
                                  nil)
                                3000))
                      ;; (ps:ps* `(defun fetch-messages ()
                      ;;            (ps:chain console
                      ;;                      (log "Fetching fresh messages"))
                      ;;            (initiate-action ,action-code)
                      ;;            nil))
                      ))
         (with-html
           (:script (:raw action))
           (cond
             ((messages widget)
              (:div :class "messages"
                    (mapc #'render (messages widget)))
              (scroll-to (lastcar (messages widget))
                         :smooth nil))
             (t
              (:p "В этом чате пока нет сообщений. Стань первым!")))
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
           (avatar-url (get-user-avatar author-id)))
      (multiple-value-bind (processed-message one-emoji)
          (render-message-text (chat/client:message-message msg))

        (let ((classes (append (list "message-text")
                               (when one-emoji
                                 (list "only-one-emoji")))))
          (:img :class "message-avatar"
                :src avatar-url)
          (:div :class "message-body"
                (:div :class (join " " classes)
                      (:raw processed-message))
                (:div :class "message-time"
                      since-as-str)))))))


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
           (setf message
                 (str:trim message))
           (unless (string-equal message "")
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
        :max-height 80vh
        :overflow scroll
        (.message-widget
         :display flex
         :flex-direction row
         :gap 1rem
         (.message-avatar
          :width 3rem
          :height 3rem
          :border-radius 1.5rem)
         (.message-body
          :display flex
          :flex-direction column
          :align-items flex-end
          (.message-time
           :font-size 0.7rem
           :color gray)
          (.message-text
           :background white
           :padding 0.5rem
           :border-radius 0.5rem)
          ((:and .message-text .only-one-emoji)
           :font-size 10rem)))
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
