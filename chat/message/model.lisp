(uiop:define-package #:chat/message/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id))
(in-package #:chat/message/model)


(defclass message ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (chat-id :initarg :chat-id
            :type string
            :col-type :uuid
            :reader chat-id)
   (user-id :initarg :user-id
            :type integer
            :col-type :bigint
            :reader user-id
            :documentation "ID пользователя, который написал сообщение.")
   (message :initarg :message
            :type string
            :col-type :text
            :reader chat-message
            :documentation "Текст сообщения"))
  (:documentation "Одно сообщение из чата.")
  (:table-name "chat.message")
  (:metaclass dao-table-class))

