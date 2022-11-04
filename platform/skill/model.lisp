(uiop:define-package #:platform/skill/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:common/event-bus
                #:emit-event)
  (:import-from #:function-cache
                #:defcached))
(in-package #:platform/skill/model)


(defclass skill ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (title :initarg :title
          :type string
          :col-type :text
          :accessor skill-title
          :documentation "Короткое название навыка.")
   (hard :initarg :hard
          :type boolean
          :col-type :boolean
          :accessor hard-skill-p
          :documentation "Является ли навык hard скиллом или soft."))
  (:table-name "platform.skill")
  (:metaclass dao-table-class))



(defcached (get-skill-title :timeout (* 60 60)) (skill-id)
  (getf (first (mito:retrieve-by-sql "SELECT title from platform.skill WHERE id = ?"
                                     :binds (list skill-id)))
        :title))
