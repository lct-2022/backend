(uiop:define-package #:chat/chat-member/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id))
(in-package #:chat/chat-member/model)


(defclass chat-member ()
  ((chat-id :initarg :chat-id
            :type string
            :col-type :uuid
            :reader chat-id)
   (user-id :initarg :user-id
            :type integer
            :col-type :bigint
            :reader user-id
            :documentation "ID пользователя, который состоит в чате.

                            Для каждого члена команды будет такая запись,
                            чтобы мы могли быстро определять,
                            кто может писать в приватный чат, а кто нет."))
  (:primary-key chat-id user-id)
  (:documentation "Привязка пользователя к чату.")
  (:table-name "chat.chat_member")
  (:metaclass dao-table-class))

