(uiop:define-package #:chat/chat-member/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/chat-member/model
                #:chat-member)
  (:import-from #:mito
                #:object-id
                #:create-dao))
(in-package #:chat/chat-member/api)


(define-rpc-method (chat-api get-members) (chat-id)
  (:summary "Возвращает user-id всех участников чата.")
  (:param chat-id integer)
  (:result chat-member)

  (with-connection ()
    (retrieve-dao 'chat-member
                  :chat-id chat-id)))
