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
       :col-type :integer
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
          :accessor job-title)
   (description :initarg :description
                :initform ""
                :type string
                :col-type :text
                :accessor job-description))
  (:table-name "platform.job")
  (:metaclass dao-table-class))
