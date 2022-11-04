(uiop:define-package #:platform/project/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:common/event-bus
                #:emit-event)
  (:import-from #:serapeum
                #:soft-list-of))
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
   (stage-id :initarg :stage-id
             :initform 1
             :type integer
             :col-type :bigint
             :accessor project-stage-id)
   (stage-title :initarg :stage-title
                :initform 1
                :type string
                :ghost t
                :accessor project-stage-title)
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
   (industry :initarg :industry
             :initform ""
             :type string
             :col-type :text
             :accessor project-industry
             :documentation "Индустрия, к которой относится проект (значения можно брать из метода get-industries).")
   (innovation-type :initarg :innovation-type
                    :initform ""
                    :type string
                    :col-type :text
                    :accessor project-innovation-type
                    :documentation "Категория инновационности (значения можно брать из метода get-innovation-types).")
   (innovations :initarg :innovations
                :initform ""
                :type string
                :col-type :text
                :accessor project-innovations
                :documentation "Описание инновационной идеи проекта.")
   (jobs :initarg :jobs
         :initform nil
         :type list
         ;; TODO: придумать, что делать с циклической зависимостью между модулями
         ;; :type (soft-list-of job)
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


