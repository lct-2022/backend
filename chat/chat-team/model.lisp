(uiop:define-package #:chat/chat-team/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id))
(in-package #:chat/chat-team/model)


(defclass chat-team ()
  ((chat-id :initarg :chat-id
            :type string
            :col-type :uuid
            :reader chat-id)
   (team-id :initarg :team-id
            :type integer
            :col-type :bigint
            :reader team-id
            :documentation "ID команды, к которой привязан чат. Члены этой команды будут иметь в чате особый доступ."))
  (:primary-key chat-id team-id)
  (:documentation "Привязка чата к команде.")
  (:table-name "chat.chat_team")
  (:metaclass dao-table-class))
