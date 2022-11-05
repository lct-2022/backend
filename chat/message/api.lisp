(uiop:define-package #:chat/message/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/message/model
                #:message)
  (:import-from #:mito
                #:object-id
                #:create-dao)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:alexandria
                #:lastcar))
(in-package #:chat/message/api)


(define-rpc-method (chat-api post) (chat-id message)
  (:summary "Добавляет в чат сообщение от текущего пользователя.")
  (:param chat-id string)
  (:param message string)
  (:result message)

  (with-session (user-id)
    (with-connection ()
      (create-dao 'message
                  :chat-id chat-id
                  :user-id user-id
                  :message message))))


(define-rpc-method (chat-api get-messages) (chat-id &key (limit 100) page-key)
  (:summary "Отдаёт сообщения из чата, с пейджинацией.")
  (:description "После того, как метод отдал пустую страницу, можно периодически
вызывать его с тем же page-key, чтобы дождаться появления новых сообщений.

По-хорошему, в проде надо будет вместо поллинга использовать websocket или server-side-events.")
  (:param chat-id string)
  (:param limit integer)
  (:param page-key integer)
  (:result (paginated-list-of message))

  (with-connection ()
    (let ((results
            (select-dao 'message
              (if page-key
                  (where (:and (:= :chat_id chat-id)
                               (:> :id page-key)) )
                  (where (:= :chat_id chat-id) ))
              (order-by :id)
              (limit limit))))
      (when results
        (let ((last-message (lastcar results)))
          (values results
                  (object-id last-message)))))))
