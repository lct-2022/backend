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
   (private :initarg :private
            :type string
            :col-type :boolean
            :reader chat-private-p
            :documentation "Тип чата. Если private, то писать/читать его могут только члены команды."))
  (:documentation "Информация о чате.")
  (:table-name "chat.chat")
  (:metaclass dao-table-class))
