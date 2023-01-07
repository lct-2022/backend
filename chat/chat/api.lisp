(uiop:define-package #:chat/chat/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/chat/model
                #:chat-archived-p
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
                #:create-dao)
  (:import-from #:serapeum
                #:fmt))
(in-package #:chat/chat/api)


(define-rpc-method (chat-api create-chat) (&key private team-id title)
  (:summary "Создаёт новый чат для команды.")
  (:description "Если private True, то в чат смогут писать не только члены команды, но и кто угодно.")
  (:param private boolean
          "Если выставить в True, то в чат смогут писать только члены команды.")
  (:param team-id integer)
  (:param title string)
  (:result chat)

  (with-connection ()
    (let* ((chat (create-dao 'chat :id (make-uuid)
                                   :private private
                                   :title title))
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


(define-rpc-method (chat-api get-chat) (id)
  (:summary "Запрашивает данные о чате.")
  (:description "Если чат не найден, то возвращает ошибку.")
  (:param id string)
  (:result chat)

  (handler-bind
      ((error (lambda (err)
                (unless (typep err 'jsonrpc:jsonrpc-error)
                  (openrpc-server:return-error (fmt "Ошибка: ~S" err)
                                               :code 11)))))
    ;; Провалидируем что id это корректный uuid
    (uuid:make-uuid-from-string id)

    (with-connection ()
      (let ((chat (find-dao 'chat :id id)))
        (cond
          (chat chat)
          (t
           (openrpc-server:return-error (fmt "Чат с id ~S не найден." id)
                                        :code 10)))))))

(define-rpc-method (chat-api archive-chat) (id)
  (:summary "Архивирует чат.")
  (:description "Если чат не найден, то возвращает ошибку.")
  (:param id string)
  (:result chat)

  (handler-bind
      ((error (lambda (err)
                (unless (typep err 'jsonrpc:jsonrpc-error)
                  (openrpc-server:return-error (fmt "Ошибка: ~S" err)
                                               :code 11)))))
    ;; Провалидируем что id это корректный uuid
    (uuid:make-uuid-from-string id)

    (with-connection ()
      (let ((chat (find-dao 'chat :id id)))
        (cond
          (chat
           (setf (chat-archived-p chat)
                 t)
           (mito:save-dao chat)
           chat)
          (t
           (openrpc-server:return-error (fmt "Чат с id ~S не найден." id)
                                        :code 10)))))))
