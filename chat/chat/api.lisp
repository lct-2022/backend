(uiop:define-package #:chat/chat/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/chat/model
                #:chat)
  (:import-from #:chat/chat-member/model
                #:chat-member)
  (:import-from #:chat/chat-team/model
                #:chat-team)
  (:import-from #:platform/client
                #:get-team-members
                #:team-member-user-id
                #:make-platform)
  (:import-from #:mito
                #:object-id
                #:create-dao))
(in-package #:chat/chat/api)


(define-rpc-method (chat-api create-chat) (&key private team-id)
  (:summary "Создаёт новый чат для команды.")
  (:description "Если private True, то в чат смогут писать не только члены команды, но и кто угодно.")
  (:param private boolean
          "Если выставить в True, то в чат смогут писать только члены команды.")
  (:param team-id integer)
  (:result chat)

  (with-connection ()
    (let* ((chat (create-dao 'chat :id (make-uuid)
                                   :private private))
           (chat-id (object-id chat)))
      (when team-id
        (create-dao 'chat-team
                    :chat-id chat-id
                    :team-id team-id)
        
        (let* ((platform (platform/client::connect (make-platform)))
               (team-members (get-team-members platform :team-id team-id)))
          (loop for member in team-members
                for user-id = (team-member-user-id member)
                do (create-dao 'chat-member
                               :chat-id chat-id
                               :user-id user-id))))
      (values chat))))
