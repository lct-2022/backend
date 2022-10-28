(uiop:define-package #:platform/team-member/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:platform/job/model
                #:job))
(in-package #:platform/team-member/model)


(defclass team-member ()
  ((id :initarg :id
       :type integer
       :col-type :integer
       :primary-key t
       :accessor object-id)
   ;; Поскольку мы тут попилили всё на микросервисы, то мы не должны
   ;; импортировать модель user из сервиса passport, а вместо этого будем отдавать из API только id.
   ;; Дальше фронт по этим id возьмёт информацию о профилях, если нужно.
   (user-id :initarg :user-id
            :type integer
            :col-type :integer
            :accessor team-member-user-id)
   (job :initarg :job
        :type job
        :col-type job
        :documentation "Член команды всегда должен соответствовать какой-то позиции в команде."
        :accessor team-member-job))
  (:table-name "platform.team_member")
  (:metaclass dao-table-class))
