(uiop:define-package #:rating/vote/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id))
(in-package #:rating/vote/model)


(defparameter *supported-subject-types*
  '("project" "user" "job" "service" "team"))


(defclass vote ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (user-id :initarg :user-id
            :type integer
            :col-type :integer
            :reader user-id)
   (subject-type :initarg :subject-type
                 :type string
                 :col-type :text
                 :reader subject-type
                 :documentation "Тип объекта, за который оставлен голос. В базе это ENUM (project, user)")
   (subject-id :initarg :subject-id
               :type integer
               :col-type :integer
               :reader subject-id
               :documentation "ID объекта, за который оставлен голос."))
  (:table-name "rating.vote")
  (:metaclass dao-table-class))
