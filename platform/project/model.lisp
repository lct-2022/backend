(uiop:define-package #:platform/project/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id))
(in-package #:platform/project/model)


(defclass project ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   ;; Поскольку мы тут попилили всё на микросервисы, то мы не должны
   ;; импортировать модель user из сервиса passport, а вместо этого будем отдавать из API только id.
   ;; Дальше фронт по этим id возьмёт информацию о профилях, если нужно.
   (author-id :initarg :author-id
              :type integer
              :col-type :integer
              :accessor project-author-id
              :documentation "Идентификатор профиля автора проекта.")
   (title :initarg :title
          :initform ""
          :type string
          :col-type :text
          :accessor project-title)
   (description :initarg :description
                :initform ""
                :type string
                :col-type :text
                :accessor project-description)
   (url :initarg :url
        :initform ""
        :type string
        :col-type :text
        :accessor project-url)
   (contests :initarg :contests
             :initform ""
             :type string
             :col-type :text
             :accessor project-contests
             :documentation "Текстовое описание конкурсов и хакатонов, в которых принимал участие."))
  (:table-name "platform.project")
  (:metaclass dao-table-class))



(defclass test-project ()
  ((ID :initarg :id
       :type integer
       :col-type :serial
       :primary-key t
       :accessor mito.dao.mixin::%object-id)
   (title :initarg :title
          :initform ""
          :type string
          :col-type :text
          :accessor project-title))
  (:table-name "platform.test_project")
  (:metaclass dao-table-class))


