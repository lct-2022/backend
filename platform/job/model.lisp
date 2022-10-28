(uiop:define-package #:platform/job/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:platform/team/model
                #:team))
(in-package #:platform/job/model)


(defclass job ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (team :initarg :team
         :type team
         :col-type team
         :accessor job-team)
   (title :initarg :title
          :initform ""
          :type string
          :col-type :text
          :accessor job-title
          :documentation "Должность.")
   (description :initarg :description
                :initform ""
                :type string
                :col-type :text
                :accessor job-description
                :documentation "Описание вакансии."))
  (:table-name "platform.job")
  (:metaclass dao-table-class))
