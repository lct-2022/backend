(uiop:define-package #:platform/team/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:platform/project/model
                #:project))
(in-package #:platform/team/model)


(defclass team ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (title :initarg :title
          :initform "Основная команда"
          :type string
          :col-type :text
          :accessor team-title)
   (project :initarg :project
            :type project
            :col-type project
            :accessor team-project))
  (:table-name "platform.team")
  (:metaclass dao-table-class))


(defgeneric get-team-id (obj)
  (:documentation "Возвращает id команды, которой принадлежит объект. Например так можно по job-application узнать id команды в которую подана заявка.")
  (:method ((obj team))
    (mito:object-id obj)))
