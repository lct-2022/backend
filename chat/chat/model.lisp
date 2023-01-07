(uiop:define-package #:chat/chat/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id))
(in-package #:chat/chat/model)


(defclass chat ()
  ((id :initarg :id
       :type string
       :col-type :uuid
       :primary-key t
       :accessor object-id)
   (title :initarg :title
          :type string
          :col-type (or :null :text)
          :accessor chat-title)
   (private :initarg :private
            :type boolean
            :col-type :boolean
            :reader chat-private-p
            :documentation "Тип чата. Если private, то писать/читать его могут только члены команды.")
   (archived :initarg :archived
             :type boolean
             :col-type :boolean
             :accessor chat-archived-p
             :documentation "Признак, заархивирован ли чат."))
  (:documentation "Информация о чате.")
  (:table-name "chat.chat")
  (:metaclass dao-table-class))
