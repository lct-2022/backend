(uiop:define-package #:platform/stage/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:common/event-bus
                #:emit-event)
  (:import-from #:function-cache
                #:defcached))
(in-package #:platform/stage/model)


(defclass project-stage ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (title :initarg :title
              :type string
              :col-type :text
              :accessor stage-title
              :documentation "Короткое название этапа развития проекта.")
   (description :initarg :description
                :initform ""
                :type string
                :col-type :text
                :accessor project-description))
  (:table-name "platform.project_stage")
  (:metaclass dao-table-class))



(defcached (get-stage-title :timeout (* 60 60)) (stage-id)
  (getf (first (mito:retrieve-by-sql "SELECT title from platform.project_stage WHERE id = ?"
                                     :binds (list stage-id)))
        :title))
