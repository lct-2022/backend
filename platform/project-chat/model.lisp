(uiop:define-package #:platform/project-chat/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:platform/project/model
                #:project))
(in-package #:platform/project-chat/model)


(defclass project-chat ()
  ((project :initarg :project
            :type project
            :col-type project
            :reader chat-project)
   (chat-id :initarg :chat-id
            :type string
            :col-type :uuid
            :reader chat-id)
   (private :initarg :private
            :type boolean
            :col-type :boolean
            :reader chat-private-p))
  (:primary-key project chat-id)
  (:table-name "platform.project_chat")
  (:metaclass dao-table-class))
