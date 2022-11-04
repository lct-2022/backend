(uiop:define-package #:platform/profession/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:common/event-bus
                #:emit-event)
  (:import-from #:function-cache
                #:defcached))
(in-package #:platform/profession/model)


(defclass profession ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (title :initarg :title
          :type string
          :col-type :text
          :accessor stage-title
          :documentation "Короткое название профессии."))
  (:table-name "platform.profession")
  (:metaclass dao-table-class))



(defcached (get-profession-title :timeout (* 60 60)) (profession-id)
  (getf (first (mito:retrieve-by-sql "SELECT title from platform.profession WHERE id = ?"
                                     :binds (list profession-id)))
        :title))
