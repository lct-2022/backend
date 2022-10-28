(uiop:define-package #:platform/job-application/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:platform/job/model
                #:job)
  (:import-from #:common/permissions
                #:assert-can-modify))
(in-package #:platform/job-application/model)


(defclass job-application ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   ;; Поскольку мы тут попилили всё на микросервисы, то мы не должны
   ;; импортировать модель user из сервиса passport, а вместо этого будем отдавать из API только id.
   ;; Дальше фронт по этим id возьмёт информацию о профилях, если нужно.
   (user-id :initarg :user-id
            :type integer
            :col-type :integer
            :accessor application-user-id)
   (job :initarg :job
        :type job
        :col-type job
        :accessor application-job)
   (message :initarg :message
            :initform ""
            :type string
            :col-type :text
            :documentation "Сообщение от пользователя, который подал заявку на вакансию."
            :accessor application-message)
   (status :initarg :status
           :initform "applied"
           :type string
           :col-type :text
           :documentation "Строка из набора \"applied\", \"accepted\", \"declined\"."
           :accessor application-status))
  (:table-name "platform.job_application")
  (:metaclass dao-table-class))


(defmethod assert-can-modify ((user-id integer) (obj job-application))
  (let* ((rows (mito:retrieve-by-sql
                "select author_id
      from platform.project as p
      join platform.team as t on p.id = t.project_id
      join platform.job as j on t.id = j.team_id
     where j.id = ?"
                :binds (list (slot-value obj 'job-id))))
         (project-owner-id
           (getf
            (first rows)
            :author-id)))
    (equal user-id project-owner-id)))
