(uiop:define-package #:platform/project/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:common/event-bus
                #:emit-event))
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
             :documentation "Текстовое описание конкурсов и хакатонов, в которых принимал участие.")
   (jobs :initarg :jobs
         :initform nil
         :type list
         :ghost t
         :accessor project-jobs
         :documentation "Список открытых вакансий. Будет заполнен если в ручку передан additional-fields = [\"jobs\"]")
   (team-size :initarg :team-size
              :initform nil
              :type (or null integer)
              :ghost t
              :accessor project-team-size
              :documentation "Суммарное количество человек, работающих над проектом. Будет заполнен если в ручку передан additional-fields = [\"team-size\"]"))
  (:table-name "platform.project")
  (:metaclass dao-table-class))


(defclass project-with-rating ()
  ((project :initarg :project
            :type project)
   (rating :initarg :rating
           :type integer)))


